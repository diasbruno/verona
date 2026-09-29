{
  description = "Termis compiler development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f (import nixpkgs {
            inherit system;
          })
        );
    in
    {
      devShells = forAllSystems (
        pkgs:
        {
          default =
            let
              clLLVM = builtins.fetchGit {
                url = "https://github.com/diasbruno/CL-LLVM.git";
                # diasbruno/CL-LLVM's LLVM 23 update branch.  Keep this
                # revision and LLVM in lockstep: it uses opaque-pointer APIs.
                rev = "cea0ba46ee61b3e7f7237d0aaad8b83bcf926e30";
              };
              lisp = pkgs.sbcl.withPackages (ps: [
                ps.fiveam
                ps.cffi
                ps.cffi-grovel
                ps.trivial-features
                ps.cl-ppcre
                ps.split-sequence
                ps.trivial-shell
              ]);
            in
            pkgs.mkShell {
              packages = [
                pkgs.cmake
                lisp
                pkgs.llvmPackages_23.llvm
                pkgs.llvmPackages_23.clang
              ];

              shellHook = ''
                export TERMIS_CL_LLVM="${clLLVM}"
                # The compiler driver supplies the platform startup objects
                # and defaults needed to turn a Termis object into a process.
                export TERMIS_LINKER="${pkgs.llvmPackages_23.clang}/bin/clang"
                export LD_LIBRARY_PATH="${pkgs.llvmPackages_23.llvm.lib}/lib:''${LD_LIBRARY_PATH:-}"
                export DYLD_LIBRARY_PATH="${pkgs.llvmPackages_23.llvm.lib}/lib:''${DYLD_LIBRARY_PATH:-}"
                echo "Termis development shell: SBCL $(sbcl --version)"
                echo "LLVM $(llvm-config --version), CL-LLVM cea0ba4"
              '';
            };
        }
      );
    };
}
