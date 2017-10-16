#!/bin/sh
systemctl --user restart emacs.service
emacsclient --no-wait
