#!/bin/sh

rm index.html *.js *.css &&\
 mv dist/* . &&\
 git add . &&\
 git ci -m 'upd' &&\
 git push &&\
 git co master
