-- | About me component, displaying information about who I am.
module AboutMe where

import Prelude
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (a, blockquote_, code_, em_, h1, hr_, li_, p_, strong_, text, ul_)
import Halogen.HTML as H
import Halogen.HTML.Properties (href, id)

type State
  = ( age :: Int )

aboutMeComponent :: forall query input output m. Component query input output m
aboutMeComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval

  initialState _ = { age: 29 } -- FIXME

  render state =
    H.div [ id "home", cl [ "content" ] ]
      [ h1 [ cl [ "title" ] ] [ text "Welcome!" ]
      , hr_
      , p_
          [ text
              """Welcome on my official website. This place is dedicated to what I do on my spare time and has
              nothing to do with any company nor professional purpose. You’ll find only personal-related projects
              and material. Feel free to have a look at the articles I write """
          , a [ href "https://phaazon.net/blog" ] [ text "on my blog" ]
          ]
      , p_
          [ text "! However, if you are a recruiter, you might be interested in "
          , a [ href "https://github.com/phaazon" ] [ text "my GitHub page" ]
          , text " and/or contacting me:"
          ]
      , ul_
          [ li_
              [ text "Via "
              , a [ href "https://github.com/phaazon" ] [ text "GitHub" ]
              , text "."
              ]
          , li_
              [ text "Via "
              , a [ href "https://twitter.com/phaazon_" ] [ text "Twitter" ]
              , text "."
              ]
          , li_
              [ text "Via IRC – as "
              , code_ [ text "phaazon" ]
              , text ", "
              , code_ [ text "phz" ]
              , text " or similar – on:"
              , ul_
                  [ li_ [ text "Libera." ]
                  , li_ [ text "Mozilla (RIP :()." ]
                  , li_ [ text "OFTC" ]
                  ]
              ]
          , li_
              [ text "Via "
              , a [ href "https://www.linkedin.com/in/dimitri-sabadie-97a9009b" ] [ text "LinkedIn" ]
              , text "."
              ]
          ]
      , p_
          [ text "I don’t update the portfolio very often; I highly suggest you to have a look at "
          , a [ href "https://github.com/phaazon" ] [ text "my GitHub page" ]
          , text
              """ for a more accurate idea of my contributions. Also, I’ve been making special efforts to decentrialize
              my my activity, so here are a few more places where you can find work of mine:"""
          ]
      , ul_
          [ li_ [ text "My own personal ", a [ href "https://git.phaazon.net" ] [ text "Gitea instance" ], text "." ]
          , li_
              [ text "My "
              , a [ href "https://hackage.haskell.org/user/DimitriSabadie" ] [ text "packages on Hackage" ]
              , text "."
              ]
          , li_
              [ text "My "
              , a [ href "https://crates.io/users/phaazon" ] [ text "creates on crates.io" ]
              , text "."
              ]
          ]
      , h1 [ cl [ "title" ] ] [ text "About me…" ]
      , hr_
      , p_
          [ text "I’m "
          , strong_ [ text "Dimitri Sabadie" ]
          , text (", I’m " <> show state.age <> "years old")
          , text
              "I was born in the South-West of France, in the neighborhood of Bordeaux. You can find me on the net as "
          , strong_ [ text "phaazon" ]
          , text ", "
          , strong_ [ text "phz" ]
          , text " or "
          , strong_ [ text "skypers" ]
          , text
              """ (old nickname). I’ve been writing programs for a
            very long time. I discovered the C language when I was 12, and started a long trek in the IT jungle. I quickly
            started getting more and more interested in C++ then I wrote a few programs in C, like an .OBJ mesh file
            loader (shamefuly called lobj) with light and camera support in OpenGL. A few days after lobj was released,
            I got my first way into the IRC world. I found a french OpenGL channel where I was advised to transform lobj
            into a realtime 3D engine. I really enjoyed my experiments with the engines I wrote. The first one was called
            SGLE for Skypers OpenGL Engine, then I refactored it again and again, and finally, I ended up in the
            demoscene world where I met nice people. It was also the time when I discovered the D language, a
            very nice language."""
          ]
      , p_
          [ text "I released two demoscene productions in 2013: "
          , strong_ [ text "Lightning Road To Liquid Radiator" ]
          , text " – a "
          , em_ [ text "PC Linux 64k intro" ]
          , text " – and "
          , strong_ [ text "Heat Station" ]
          , text " – a "
          , em_ [ text "PC Windows 64k intro" ]
          , text ", at Eersel (NL)  and Köln (DE) respectively."
          ]
      , p_
          [ text "I then had several years learning and writing programs in "
          , strong_ [ text "Haskell" ]
          , text " – I still am! That period was a very important one to me as I learned a lot about "
          , em_ [ text "functional programming" ]
          , text ", and especially "
          , em_ [ text "pure functional programming" ]
          , text ". I’ve been introducing myself as a "
          , em_ [ text "functional programmer" ]
          , text " ever since. My "
          , strong_ [ text "Haskell" ]
          , text
              """ experience taught me a lot and helped me sharpening
            my mind and rigorous thoughts about software engineering, language designs and software architectures"""
          ]
      , p_
          [ text "After those years, I decided to get back to writing demo productions and decided to jump in"
          , a [ href "https://www.rust-lang.org" ] [ text "Rust" ]
          , text " as it’s a very well appreciated language in the "
          , strong_ [ text "Haskell" ]
          , text
              """ community. I learned the basic language in two days and the more complex concepts in a week
          (lifetime ellision, borrowing, mpsc, complex macros, uniqueness typing, etc.). My """
          , strong_ [ text "Haskell" ]
          , text " experience helped a lot to get my feet wet with "
          , strong_ [ text "Rust" ]
          , text ". Today, I consider "
          , strong_ [ text "Rust" ]
          , text " as "
          , em_ [ text "“animperative Haskell”" ]
          , text
              """. Less powerful in terms of abstraction, it’s better to handle memory and design the control-flow of
          my programs."""
          ]
      , p_
          [ text "I’m currently sticking around "
          , strong_ [ text "Rust" ]
          , text "and "
          , strong_ [ text "Haskell" ]
          , text
              """, as they fit my needs in terms of abstraction, readability, expressiveness and minimal runtime
            overhead (thus runtime performance in terms of CPU and memory footprint), while still getting interested
            into learning new languages (lately, Idris, Elm, Go, modern Java, Scala, some Lisp and a bunch of others).
            I don’t consider myself as belonging to a specific expertise field, even though I think I’m best at graphics
            programming, code architecture and APIs designing, and clearly type-level architecture and type-system
            coding."""
          ]
      , h1 [ cl [ "title" ] ] [ text "Pro-side" ]
      , hr_
      , p_
          [ text "I obtained my engineer’s degree ("
          , a [ href "https://en.wikipedia.org/wiki/Commission_des_Titres_d%27Ing%C3%A9nieur" ] [ text "CTI" ]
          , text ") by fall 2015, delivered by "
          , a [ href "https://en.wikipedia.org/wiki/EFREI" ] [ text "EFREI" ]
          , text ". You can download a generic version of my CV "
          , a [ href "http://phaazon.net/pub/CV.pdf" ] [ text "here" ]
          , text "."
          ]
      , p_
          [ text "Today, I work at "
          , a [ href "https://www.datadoghq.com" ] [ text "Datadog" ]
          , text " as Software Engineer. I work on distributed and highly available data systems."
          ]
      , blockquote_
          [ p_
              [ text
                  """Disclaimer to headhunters: if you want to contact me for a job offer, please do not use a template
              you send to everyone. It’s both boring and disrespectful. Even though I used to respond to that kind of
              requests, I will not anymore. What would you think if someone applied for a job position with a template
              they would obviously use for other companies? Do you feel they would express their real interest in
              getting hired in your company with such a process? If you really want to contact me, just talk to me with
              your own words and read through my work.. I’m a human being, I don’t want to talk to a template nor being
              considered as “a human resource.”"""
              ]
          , p_
              [ text "Disclaimer²: I’m not interested in PHP. You peeps have to stop with that."
              ]
          ]
      , p_
          [ text
              "Enjoy your visit, and feel free to contact me!"
          ]
      ]
