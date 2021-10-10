#!/bin/bash
if [ "$1" = "on" ]; then
    cp ${HOME}/Library/KeyBindings/del-DefaultKeyBinding.dict ${HOME}/Library/KeyBindings/DefaultKeyBinding.dict
else
    cp ${HOME}/Library/KeyBindings/def-DefaultKeyBinding.dict ${HOME}/Library/KeyBindings/DefaultKeyBinding.dict
fi
