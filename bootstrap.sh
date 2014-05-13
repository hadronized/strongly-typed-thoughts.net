#!/bin/bash

cabal sandbox init
cabal install mtl happstack-server blaze-html text pandoc data-default filepath directory
