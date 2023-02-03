# Dotfiles

This repository is a collection of configuration files (also known as dotfiles) that can be managed using [GNU Stow](https://www.gnu.org/software/stow/).

## What are dotfiles?

Dotfiles are configuration files in Unix-based systems that are used to customize the system’s behavior and appearance. These files typically start with a dot (.) and are stored in the home directory.

## Why use GNU Stow?

GNU Stow is a symlink manager that allows you to manage your dotfiles in a clean and organized way. It makes it easy to manage and switch between different configurations, as well as keep track of changes to your configuration files in a version control system like Git.

## How to use this repository

1. Install GNU Stow using Homebrew:
  
   ```sh
   brew install stow
   ```
   
2. Clone this repository to your local machine:
 
   ```sh
   git clone https://github.com/devinschulz/dotfiles.git
   ```
   
3. Navigate to the repository directory:
 
   ```sh
   cd dotfiles
   ```
   
4. Use GNU Stow to symlink the desired configuration files to your home directory:
 
   ```sh
   stow <package>
   ```
   
   Replace `<package>` with the name of the package you want to symlink (e.g. `nvim`, `fish`, etc.).

Repeat step 4 for each package you want to install.

## Tips

- Before installing a new package, it is recommended to backup any existing configuration files in your home directory to avoid conflicts.
- To remove a package, simply run `stow -D <package>` in the repository directory.
