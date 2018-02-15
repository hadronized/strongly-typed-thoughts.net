# Volumetric light shafts

On the last weekend, I was at the Revision demoparty (Easterparty). It’s a
meeting of friends and great people doing graphics, music, code and that kind
of fun and pretty stuff. I didn’t release anything, but I’ve been working on
a release for a while now, and I was lacking something in my engine: *light 
shafts*.

I know a few ways to do that. They almost all fall in one of the two following
classes:

  1. screen-space-based;
  2. raymarching-based.

Both the techniques produce interesting images. However, the *screen-space-based*
method produces bad results when you don’t look directly to the light – it may
even produce nothing if the light is out of screen – and the *raymarching-based*
is a step process that might generate artifacts and can be slow.

The idea I had is **very simple**. I haven’t tested it yet, but it’s planned for
very soon. I’ll post images with benchmarks as soon as I have something on screen.
I’m not sure it’s an unknown way to do it. I just haven’t found anything
describing that yet. If you have, please leave a link in the comments below! :)

# Volume extraction

The idea is the following. You need a depthmap. If you’re used to *shadow mapping*
you already have a depthmap for your light. In order to simplify this, we’ll use the
depthmap used for shadow mapping, but I guess it’s totally okay to use another
depthmap – we’ll see that it could be even handier.

For each point in that depthmap is the distance – in a specific space coordinates
system – of the corresponding point in world space to the 
position of the light. If you have the projection and the view matrices of the
light, it’s easy to deproject the depthmap. What would we get if we deproject
**all the depthmap texels** into the world space? We’d get the exact lit surfaces.

For a spot light, you can imagine the deprojected version of the depthmap as a cone
of light. The “disk” of the cone will deform and shape as the lit environment. That’s
the first part of the algorithm.

We have a points cloud. What happens if, for each deprojected texel – i.e. point – we
draw a line to the position of the light? We get an actual lines field representing…
photons paths! How amazing is that?! Furthermore, because of the depthmap sampling,
if you look at the light and that an object is between you and the light, the photons
paths won’t go through the obstructing object! Like the following image:

![](http://i280.photobucket.com/albums/kk167/Bea_Douglas/ShaftsofLightMountBaker-SnoqualmieN.jpg)

Of course, because of using raw lines, the render might be a bit blocky at first. If you
know the *laser trick*[^1] – i.e. quad-based lasers, you can apply it to our lines as
well, in order to get better results. The first improvement is to disable depth
test and enable additive blending.

# Algorithm

In the first place, you need to generate the depthmap of the light. Then, you need to extract
the volumetric shaft. You’ll need a vertex buffer for that. Allocate **w\*h** elements, where
*w* and *h* are the depthmap’s *width* and *height*. Yeah, a point per texel.

Create a shader and make a render with `glDrawArrays(GL_POINTS, 0, w*h)` with an *attributeless
vertex array object*. Don’t forget to bind the depthmap for use in the shader.

In your vertex shader, use `gl_VertexID` to sample from the depthmap. Transform the resulting
texel in world-space. You can use something like the following deprojection formula:

```C
vec3 deproject() {
  float depth = 2. * texelFetch(depthmap, ivec2(gl_FragCoord.xy), 0).r - 1.;
  vec4 position = vec4(vv, depth, 1.);
  position = iProjView * position;
  position.xyz /= position.w;
  return position.xyz;
}
```

Pass that to the next stage, the geometry shader. There, build whatever kind of new primitive
you want. In the first place, I’ll go for a simple line connected to the light’s position. In
further implementation, I’ll go for lasers-like base shapes, like star-crossed quads.

In the fragment shader, put whatever color you want the shaft to have. You could use 
interpolation to reduce lighting wherever you want to create nice effects.

Don’t forget to use additive blending, as we do for lasers.

# Considerations

I see two major problems. The first one is the bright aspect the shaft will have if you don’t
blend correctly. Play with alpha to reduce more if the fragment is near the light’s position and
make the alpha bigger when you’re far away from the light’s position. Because you’ll blend way more
photons paths near the light’s position than far from it.

The second issue is the resolution of the extracted volume. For a 512x512 depthmap, you’ll get
around 262k points, then 524k lines. That’s a lot for such an object. And that’s only for a simple
spot light. An omnidirectional light would require six times more lines. What happens if we don’t
use lines, but star-crossed quads an that we want several shafts? You see the problem.

A solution could be to sample from high mipmap level, so that you don’t use the full resolution of
the shadow map. That would result in less visual appealing shafts, but I’m pretty sure it’d be still
good. You could also branch a blur shader to smooth out the whole thing.

# Conclusion

I’ll try to implement that as soon as possible, because I think my idea is pretty interesting compared
to raymarching, which is expensive, and way better than screen-space, because the shaft will still be
visible if the light goes out of screen.



[^1]: I’ll write an article about it if you don’t – leave a comment for asking
