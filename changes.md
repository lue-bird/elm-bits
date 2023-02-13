# changes log

#### 6.0.2

  - `typesafe-array` dependency → >= 29.1.0
  - `bounded-nat` dependency → >= 33.1.0
  - `linear-direction` dependency → >= 10.0.0
  - `atMost` name → `takeAtMost`

#### 6.0.1

  - `typesafe-array` dependency → >= 27.1.1
  - `linear-direction` dependency → >= 8.0.0

## 6.0.0

  - `typesafe-array` dependency → >= 27.0.0
  - `Bit.fuzz` add
      - `test` dependency add
  - `bounded-nat` dependency → >= 29.0.0

## 5.0.0

- `module Bit`
    - `serialize` remove
    - `toNat` → `toN`
    - `fromN` add
    - `opposite` add
- `module Bits`
    - `serialize` remove
    - `random` remove
        - in favor of `ArraySized.random Bit.random`
    - `toNat` name → `toN`
    - `toBytes` remove
        - in favor of `toChunksOf n8`
    - `padToByte` remove
        - in favor of `padToLength n8`
    - `padToLength`, `toChunksOf` add
    - `fromN` add
    - `fromIntSigned`, `toIntSigned` add

#### 4.0.2

- updated `typed-value` to 6.0.0

#### 4.0.1

- updated `bounded-nat` to 20.0.0

## 4.0.0

- updated `bounded-nat` to 19.0.0
- updated `typesafe-array` to 18.0.0

#### 3.0.1

- corrected `Arr.extend` to `InArr.append` in examples

## 3.0.0

- updated `typesafe-array` to 16.0.0
    - `Bits.serialize` now uses a custom error instead of a `String`

#### 2.0.8

- updated `typesafe-array` to 15.0.0

#### 2.0.7

- updated `typesafe-array` to 14.0.0

#### 2.0.6

- updated `bounded-nat` to 18.0.0

#### 2.0.5

- updated `bounded-nat` to 17.0.0 & `typesafe-array` to 13.0.0

#### 2.0.4

- corrected old `Arr.extendOnly`s to `extend`s in the readme

#### 2.0.3

- updated `typesafe-array` to 12.0.0
- updated bit representation website

#### 2.0.2

- updated `bounded-nat` to 15.0.0 & `typesafe-array` to 10.0.0

#### 2.0.1

- updated `bounded-nat` to 12.0.0 & `typesafe-array` to 8.0.0

## 2.0.0

- renamed `Bit.generate` to `Bit.random`
- corrected `Uuid.generate` example function & renamed it to `random`
