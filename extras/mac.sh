#!/usr/bin/env bash

#
# General UI/UX
#

# Disable automatic capitalization as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

# Disable smart dashes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable automatic period substitution as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

# Disable smart quotes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

#
# Dock, Input, Spaces, and Keyboard
#

# Use the Fn key to switch between keyboard layouts for writing in other languages
defaults write com.apple.HIToolbox AppleFnUsageType -int "1"

# Repeats the key as long as it is held down.
defaults write NSGlobalDomain "ApplePressAndHoldEnabled" -bool "false"

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

# Lock the Dock size
defaults write com.apple.Dock size-immutable -bool yes

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Don’t show recent applications in Dock
defaults write com.apple.dock show-recents -bool false

# Display the input menu in the menu bar
defaults write com.apple.TextInputMenu visible -bool true

# Restart Dock
killall Dock

echo "Done"
