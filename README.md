# Intray


## Installation 

### Cloning

Clone the repository

``` shell
git clone https://github.com/NorfairKing/intray.git --recursive
```

### Building

#### With Nix

A flake is provided in `flake.nix`.


#### With stack

``` shell
stack install intray
```

## Configuration

### Config file location

The `intray` cli application looks for config files in these locations by default, in order:

```
- $XDG_CONFIG_HOME/intray/config.yaml
- $HOME/.config/intray/config.yaml
- $HOME/.intray/config.yaml
```

Run `intray --help` to see how intray can be configured.

## Setting up synchronisation with `intray.eu`

Put this in your config file:

```
url: 'https://api.intray.eu'
username: 'YOUR USERNAME HERE'
```

Then register:

``` shell
intray register
```

and login:

``` shell
intray login
```

Now you can sync manually:

``` shell
intray sync
```

Note that syncing will occur automatically any time you change anything locally.
If you would prefer to schedule syncing manually to decrease latency locally, you can use a different syncing strategy:

```
sync: NeverSync
```


### Setting up Intray using Home Manager


See the `homeManagerModules.default` in the provided `flake.nix`.
Within your `home.nix`, add the intray module from this repository:

``` nix
let
  intrayModule = intray.homeManagerModules.${system}.default;
in
{
  imports = [
    intrayModule
  ];
  programs.intray = {
    enable = true;
    sync = {
      enable = true;
      username = "YOUR_USERNAME_HERE";
      password-file = "YOUR_PASSWORD_FILE_HERE";
    };
  };
}
```
