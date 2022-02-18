# Changelog for bitstring

## 0.1.0.0  -- 2022-02-18
  - **Note:** Some breaking changes. Please ensure that your packages
  have appropriate upper bounds of bitstring.

  - Ability to construct BitString gradually from single bits
  - Ability to create a BitString from an Integral type (and vice versa)
  - BitString is now an instance of Monoid, Bits, Ord and IsList
  - Several other functions for more versatile usage
