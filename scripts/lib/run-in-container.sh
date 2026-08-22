#!/bin/sh
#
# Sourced, not executed. The shebang is inert when sourced, but it makes file(1)
# classify this as a shell script so that scripts/shellcheck actually lints it --
# without it the file is skipped and nothing here is ever checked.

if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  # Script was run from outside the container, re-exec inside the container
  # with the same arguments
  docker compose build
  exec docker compose run --rm dev "$0" "$@"
fi
