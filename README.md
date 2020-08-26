# Dotfiles

Personal dotfiles for use on macOS systems.

## Instructions:
Either clone this directly in the `~/` directory, or copy the dotfiles over.

System packages are handled with [Homebrew][1]. That will need to be installed manually first on a new system.
Then install all packages with:
```bash
xargs brew install < homebrew.dependencies
xargs brew install < homebrew.cask.dependencies
```

After installing new homebrew packages, generate `.dependencies` files with:
```
brew leaves > homebrew.dependencies
brew cask list | sort > homebrew.cask.dependencies
```

### Terminal:
[Oh My Zsh][3] makes things nice. Manually install to ensure maximum ✨.

### Vim:
Vim plugins are managed by [vim-plug][2]. Once manually installed, bootstrap by opening vim and running `:PlugInstall`.

### Emacs:
Emacs plugins are managed by [Cask][3] and [Pallet][4]. Bootstrap by running `cask install` in the `~/.emacs.d/` directory.

[1]: https://brew.sh
[2]: https://github.com/junegunn/vim-plug
[3]: https://ohmyz.sh
[4]: https://github.com/cask/cask
[5]: https://github.com/rdallasgray/pallet
