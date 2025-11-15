# Run the rdcAVS application

The rdcAVS application deploys to the web browser locally. It consists
of three tabs that creates a campaign, add geographic information, and
set Google Drive permissions. Creating a campaign outputs a folder
prefixed with "CAMPAGNE\_", which contains a hierarchical structure,
going from the largest geographic unit (Province) to the lowest
geographic unit (Zone de Sant9). Within the Zone de Sant9 folder,
contains a template file.

## Usage

``` r
campagneApp()
```

## Value

None.

## Details

The campaign creation tab takes valid input based on what is given in
the geographic information tab. Likewise, entries in the permissions
database also depend on the geographic information tab to ensure
consistency, especially when non-global permissions are set.

Both permissions and geographic information is stored locally in a
user's machine.

## Examples

``` r
if (FALSE) { # \dontrun{
campagneApp()
} # }
```
