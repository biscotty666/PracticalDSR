{
  description = "An R flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        #pkgs = nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            R
            quarto
            chromium
            pandoc
            texlive.combined.scheme-full
            rstudio
            radianWrapper
            grass
            (with rPackages; [
              bookdown
              languageserver
              lintr
              magrittr
              tidyverse
              quarto #
              arules
              bitops
              caTools
              cdata
              DBI
              dbplyr
              DiagrammeR
              e1071
              fpc
              glmnet
              glmnetUtils
              gridExtra
              hexbin
              kernlab
              igraph
              knitr
              lime
              lubridate
              MASS
              mgcv
              pander
              plotly
              pwr
              qmrparser
              randomForest
              readr
              readxl
              rpart
              rpart_plot
              RPostgres
              rqdatatable
              rquery
              RSQLite
              scales
              sigr
              sqldf
              tidypredict
              text2vec
              vtreat
              wrapr
              WVPlots
              xgboost
              xts
              webshot
              zeallot
              zoo
              janitor
            ])
          ];
          shellHook = ''
          '';
        };
      }
    );
}
