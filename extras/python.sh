#!/bin/bash

if ! brew list | grep -q pyenv; then
  echo "pyenv could not be found. Installing..."
  brew install pyenv
fi

if ! brew list | grep -q pyenv-virtualenv; then
  echo "pyenv-virtualenv could not be found. Installing..."
  brew install pyenv-virtualenv
fi

PYTHON_VERSION=3.12.8
PLUGIN_DIR="$(pyenv root)/plugins/pyenv-update"

# Check if pyenv-update plugin directory exists
if [ ! -d "$PLUGIN_DIR" ]; then
  echo "pyenv-update plugin not found. Installing..."
  git clone https://github.com/pyenv/pyenv-update.git "$PLUGIN_DIR"
else
  echo "pyenv-update plugin found. Checking for updates..."
  cd "$PLUGIN_DIR"

  # Check if there are any changes to pull
  git remote update
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})

  if [ "$LOCAL" != "$REMOTE" ]; then
    echo "Updates available. Pulling latest changes..."
    git pull
  else
    echo "pyenv-update is up to date"
  fi
fi

if ! pyenv versions | grep -q $PYTHON_VERSION; then
  echo "Installing Python $PYTHON_VERSION..."
  pyenv install $PYTHON_VERSION
fi

pyenv global $PYTHON_VERSION
echo "Python $PYTHON_VERSION installed and set as global version"

# Update pyenv
echo "Updating pyenv..."
pyenv update
