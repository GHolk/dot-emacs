#!/bin/sh
systemctl --user restart emacs.service
emacsclient -c --no-wait
