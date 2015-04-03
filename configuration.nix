	# Edit this configuration file to define what should be installed on
	# your system.  Help is available in the configuration.nix(5) man page
	# and in the NixOS manual (accessible by running ‘nixos-help’).

	{ config, pkgs, ... }:

	{
	  imports =
	    [ # Include the results of the hardware scan.
	      ./hardware-configuration.nix
	    ];

	  # Use the GRUB 2 boot loader.
	  boot.loader.grub.enable = true;
	  boot.loader.grub.version = 2;
	  # Define on which hard drive you want to install Grub.
	  boot.loader.grub.device = "/dev/sda";

	  # networking.hostName = "nixos"; # Define your hostname.
	  networking.hostId = "bbe7081a";
	  # networking.wireless.enable = true;  # Enables wireless.

	  services.virtualboxGuest.enable = true;
	  boot.initrd.checkJournalingFS = false;

	  # Select internationalisation properties.
	  i18n = {
	     consoleFont = "lat9w-16";
	     consoleKeyMap = "es";
	     defaultLocale = "en_US.UTF-8";
	  };

	  # List packages installed in system profile. To search by name, run:
	  # $ nix-env -qaP | grep wget
	  environment.systemPackages = with pkgs; [
	    xlibs.xmodmap
	    xsel
	    wget
	    vim
	    emacs24
	    haskellPackages.xmobar
	    dmenu
	    rxvt_unicode
	    firefox
	    git
	    fish
	    tmux
	    parcellite
	    sqlite
	    postgresql
	    haskellPackages.ghc
	    haskellPackages.cabalInstall
	    haskellPackages.cabal2nix
	  ];

	  # Enable the X11 windowing system.
	  services.xserver = {
		layout = "es";
		enable = true;
		windowManager.default = "xmonad";
		desktopManager.default = "none";
		windowManager.xmonad.enable = true;
		windowManager.xmonad.enableContribAndExtras = true;
		displayManager.auto.enable = true;
		displayManager.auto.user = "natxo";
		#  services.xserver.xkbOptions = "eurosign:e";
	  };
	
	  services.postgresql = {
		enable = true;
		package = pkgs.postgresql93;
		authentication = pkgs.lib.mkForce ''
			# Generated file; do not edit!
			local all all                trust
			host  all all 127.0.0.1/32   trust
			host  all all ::1/128        trust
			host  all all 192.168.1.0/24 trust
'';
	  };


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.guest = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/natxo";
    description = "natxo";
    extraGroups = ["wheel" "networkmanager"];
  };
}
