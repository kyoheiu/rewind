# rewind - A CLI tool to downgrade packages using local Pacman cache.

![gif](https://github.com/kyoheiu/rewind/blob/main/sample.gif)

## Installation
```
git clone https://github.com/kyoheiu/rewind.git
cd rewind
cabal install
```

## Usage
```
rewind [package name you want to downgrade]
```
like `rewind neovim`.  
You can use mutliple arguments like `rewind neovim emacs` as well.
