# [Yurie/File](https://github.com/yuriever/Yurie-File)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Wolfram Language](https://img.shields.io/badge/Wolfram%20Language-14.3%2B-red.svg)](https://www.wolfram.com/language/)

File operation.


## Documentation


## Usage

1. Clone or download this repository

2. Move the entire folder to the user paclet directory:

   ```wl
   $UserBasePacletsDirectory
   ```

3. Rebuild the paclet data:

   ```wl
   PacletDataRebuild[]
   ```

4. Load the paclet

    ```wl
    Needs["Yurie`File`"]
    ```


### Uninstallation

```wl
PacletUninstall["Yurie/File"]
```


### Installation checking

```wl
PacletFind["Yurie/File"]
```