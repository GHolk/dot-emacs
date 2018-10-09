#!/bin/sh

wait_emacs_start() {
    while sleep 10s
    do
        if systemctl --user status emacs >/dev/null
        then break
        fi
    done
}

wait_emacs_start

systemctl --user restart emacs.service
emacsclient -c --no-wait
