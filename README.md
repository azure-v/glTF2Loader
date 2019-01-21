## Getting started
Add `gltf2loader` to your project's dependencies or copy gltf2.d into your source folder and then import the module: 

`import glTF2Loader;`
## Usage
### Loading an asset
To load and parse an asset:

 `glTF2Asset asset = glTF2Asset (fileName);`
 
 or
 
 `glTF2Asset asset;`
 
 `asset.load (fileName); `
 
 The `glTF2Asset` struct is reset every time a new asset is loaded.
### Reading JSON data
All JSON data is stored in arrays of structs within `glTF2Asset`. They map one-to-one to corresponding JSON objects and match their structures, with a few exceptions:

-  `extensions` and `extra` fields are ignored for the time being, since no extensions are supported as of right now;
- there are no separate `perspective` and `orthographic` objects in `glTF2Camera` struct; instead, it contains `zfar`, `znear`, `aspectRatio`, `yfov`, `xmag` and `ymag` fields, camera type is stored in an enum and unused fields are ignored;
- most importantly, `glTF2Buffer` and `glTF2Image` have a `data` field of type `void[]` where binary data is stored.
### Reading binary data
`asset.accessData (accessorIndex)` retrieves data through the specified accessor. It returns `void[]`; you're supposed to know what to do with the data.

`asset.accessSparseData (accessorIndex)` retrieves data through the specified sparse accessor. It returns `void[][uint]`; again, you're supposed to know what to do with the data.

`asset.accessImage (imageIndex)` retrieves raw data for the specified image. **NB**: image data stored in buffers is not duplicated in `glTF2Image` structs; for this reason, reading image data directly from `glTF2Image.data` is **not recommended**.

### Capabilities and spec conformance
All standard glTF2 objects are currently supported, as well as embedded buffers and .glb files. As mentioned earlier, no extensions are currently supported; if the asset specifies any required extensions, loader will output an error message to stdout and return without loading the asset (though it will still reset the `glTF2Asset` struct).

All assets from the [official sample models repository](https://github.com/KhronosGroup/glTF-Sample-Models)  should load and parse correctly (except the ones with Draco-compressed buffers).