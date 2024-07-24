{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = builtins.attrValues self.overlays;
      });
    in
    {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            name = "dev";
            buildInputs = with pkgs; [
              purs-bin.purs-0_15_15
              purs-tidy
              purs-backend-es
              spago-unstable
              purescript-language-server
              nodejs
            ];
          };
        });
    };
}
