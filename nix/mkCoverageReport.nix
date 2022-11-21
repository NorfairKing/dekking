{ stdenv }:
{ name ? "coverage", packages ? [ ] }:
stdenv.mkDerivation {
  inherit name;
}
