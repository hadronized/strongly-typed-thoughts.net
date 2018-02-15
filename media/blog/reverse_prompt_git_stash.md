It’s been a while I’m experiencing issues with `git stash`. If you don’t know that command yet,
`git stash` is used to move all the changes living in your *staging area* into a special place: the
*stash*.

The *stash* is a temporary area working like a stack. You can push changes onto it via `git stash`
or `git stash save`; you can pop changes from top with `git stash pop`. You can also apply a very
specific part of the stack with `git stash apply <stash id>`. Finally you can get the list of all
the stashes with `git stash list`.

We often use the `git stash` command to stash changes in order to make the working directory clear
again so that we can apply a patch, pull some changes, change branch, and so on. For those purposes,
the *stash* is pretty great.

However, I often forget about my stashes – I know I’m not the only one. Sometimes, I stash something
and go to cook something or just go out, and when I’m back again, I might have forgotten about what
I had stashed, especially if it was a very small change.

My current prompt for my shell, [zsh](http://www.zsh.org/), is in two parts. I set the `PS1`
environnment variable to set the regular prompt, and the `RPROMPT` environnment variable to set a
reversed prompt, starting from the right of the terminal. My reversed prompt just performs a `git`
command to check whether we’re actually in a `git` project, and get the current branch. Simple, but
nice.

I came up to the realization that I could use the exact same idea to know whether I have stashed
changes so that I never forget them! Here’s a screenshot to explain that:

![](http://phaazon.net/pub/git_stash_shell.png)

As you can see, my prompt now shows me how many stashed changes there are around!

# The code

I share the code I wrote with you. Feel free to use it, modify it and share it as well!

```
# …

function gitPrompt() {
  # git current branch
  currentBranch=`git rev-parse --abbrev-ref HEAD 2> /dev/null`
  if (($? == 0))
  then
    echo -n "%F{green}$currentBranch%f"
  fi

  # git stash
  stashNb=`git stash list 2> /dev/null | wc -l`
  if [ "$stashNb" != "0" ]
  then
    echo -n " %F{blue}($stashNb)%f"
  fi

  echo ''
}

PS1="%F{red}%n%F{cyan}@%F{magenta}%M %F{cyan}%~ %F{yellow}%% %f"
RPROMPT='$(gitPrompt)'

# …
```

Have fun!
