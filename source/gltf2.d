module glTF2Loader;

import std.stdio: writeln;
import std.conv: to;
import std.exception: enforce;
import std.file: read;
import std.path: dirName;
import std.json;
import std.base64: Base64;
import std.bitmanip: peek, Endian;
import std.algorithm.searching: findSplit, findSplitAfter;

alias Attribute = int[string];

const string[3] attributeNames = ["POSITION", "NORMAL", "TANGENT"];
const string[7] typeNames = ["SCALAR", "VEC2", "VEC3", "VEC4", "MAT2", "MAT3", "MAT4"];
const string[3] alphaModes = ["OPAQUE", "MASK", "BLEND"];
const string[4] interpolationModes = ["LINEAR", "STEP", "CUBICSPLINE"];
const string[4] targetTypes = ["translation", "rotation", "scale", "weights"];

private float getFloatValue (JSONValue value) {
	float result;
	if (value.type == JSONType.float_)
		result = value.floating;
	else
		result = cast (float) value.integer;
	
	return result;
}

struct glTF2Metadata {
    string copyright;
    string generator;
    string currVersion;
    string minVersion;
}

struct glTF2Scene {
    string name;
    uint[] nodes;
};

struct glTF2Primitive {
    int indices = -1;
    int material = -1;

    enum Mode : ubyte {
        Points = 0,
        Lines = 1,
        LineLoop = 2,
        LineStrip = 3,
        Triangles = 4,
        TriangleStrip = 5,
        TriangleFan = 6
    }
	Mode mode = Mode.Triangles;

    Attribute attributes;
    Attribute[] targets;
};

struct glTF2Mesh {
    string name;

    float[] weights;
    glTF2Primitive[] primitives;
};

struct glTF2Buffer {
    string name;

    string uri;
    ulong byteLength = 0;

    void[] data = null;
};

struct glTF2BufferView {
    string name;

    uint buffer; 
    ulong byteOffset = 0;
    ulong byteLength = 0;
    uint byteStride = 0;

    enum TargetType : ushort {
        None = 0,
        ArrayBuffer = 34962,
        ElementArrayBuffer = 34963
    } 
	TargetType target;
};

struct glTF2Texture {
    string name;

    int sampler = -1;
    int source = -1;
};

struct glTF2Sampler {
    string name;

    enum MagFilter : ushort {
        None = 0,
        Nearest = 9728,
        Linear = 9729
    }
	MagFilter magFilter = MagFilter.None;

    enum MinFilter : ushort {
        None = 0,
        Nearest = 9728,
        Linear = 9729,
        NearestMipMapNearest = 9984,
        LinearMipMapNearest = 9985,
        NearestMipMapLinear = 9986,
        LinearMipMapLinear = 9987
    } 
	MinFilter minFilter = MinFilter.None;

    enum WrappingMode : ushort {
        ClampToEdge = 33071,
        MirroredRepeat = 33648,
        Repeat = 10497
    };

    WrappingMode wrapS = WrappingMode.Repeat;
    WrappingMode wrapT = WrappingMode.Repeat;
};

struct glTF2Image {
    string name;
    string uri;
    string mimeType;
	
    int bufferView = -1;

	void[] data = null;
};

struct glTF2Material {
    string name;

    struct Texture {
        int index = -1;
        int texCoord = 0;
    };

    struct PbrMetallicRoughness {
        float[4] baseColorFactor = [1.0f, 1.0f, 1.0f, 1.0f];
        Texture baseColorTexture;

        float metallicFactor = 1.0f;
        float roughnessFactor = 1.0f;
        Texture metallicRoughnessTexture;
    } 
	PbrMetallicRoughness pbrMetallicRoughness;

    struct NormalTexture {
        int index = -1;
        ulong texCoord = 0;
        float scale = 1.0f;
    } 
	NormalTexture normalTexture;

    struct OcclusionTexture {
        int index = -1;
        ulong texCoord = 0;
        float strength = 1.0f;
    } 
	OcclusionTexture occlusionTexture;

    Texture emissiveTexture;

    float[3] emissiveFactor = [0.0f, 0.0f, 0.0f];

    enum AlphaMode : ubyte {
        Opaque,
        Mask,
        Blend
    } 
	AlphaMode alphaMode = AlphaMode.Opaque;

    float alphaCutoff = 0.5f;
    bool doubleSided = false;
};

struct glTF2Camera {
	enum CameraType : ubyte {
		Perspective,
		Orthographic
	}
	CameraType cameraType;
	
	float znear;
	float zfar = -1;
	
	float aspectRatio;
	float yfov;
	
	float xmag;
	float ymag;
}

struct glTF2Node {
    string name;

    int camera = -1;
    int mesh = -1;
    int skin = -1;

    int[] children;

    float[16] matrix = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
    float[4] rotation = [0, 0, 0, 1];
    float[3] scale = [1, 1, 1];
    float[3] translation = [0, 0, 0];

    float[] weights;
};

struct glTF2Accessor {
    string name;

    int bufferView = -1;
    ulong byteOffset = 0;
    ulong count;
    bool normalized = false;

    enum ComponentType : ushort {
        Byte = 5120,
        UnsignedByte = 5121,
        Short = 5122,
        UnsignedShort = 5123,
        UnsignedInt = 5125,
        Float = 5126
    } 
	ComponentType componentType;

    enum Type : ubyte {
        Scalar,
        Vec2,
        Vec3,
        Vec4,
        Mat2,
        Mat3,
        Mat4
    } 
	Type type;
	
	union minMaxArray {
		int[] i;
		float[] f;
	}
	minMaxArray min;
	minMaxArray max;
	
	struct Indices {
		uint bufferView;
		ComponentType componentType;
		uint byteOffset = 0;
	}
	
	struct Values {
		uint bufferView;
		uint byteOffset = 0;
	}
	
	struct Sparse {
		uint count;
		Indices indices;
		Values values;
	}
	Sparse sparse;
};

struct glTF2Skin {
	uint inverseBindMatrices;
	uint[] joints;
	uint skeleton;
}

struct glTF2Animation {
	string name;

	enum Interpolation : ubyte {
		Linear,
		Step,
		CubicSpline
	}
	struct Sampler {
		uint input;
		uint output;
		Interpolation interpolation = Interpolation.Linear;
	};
	Sampler[] samplers;
	
	enum Path : ubyte {
		Translation,
		Rotation,
		Scale,
		Weights
	}
	struct Target {
		uint node;
		Path path;
	}
	struct Channel {
		uint sampler;
		Target target;
	}
	Channel[] channels;
}

struct glTF2Asset {
	glTF2Metadata metadata;

    glTF2Accessor[] accessors;
    glTF2Animation[] animations;
    glTF2Buffer[] buffers;
    glTF2BufferView[] bufferViews;
    glTF2Camera[] cameras;
    glTF2Image[] images;
    glTF2Material[] materials;
    glTF2Mesh[] meshes;
    glTF2Node[] nodes;
    glTF2Sampler[] samplers;
    int scene = -1;
    glTF2Scene[] scenes;
    glTF2Skin[] skins;
    glTF2Texture[] textures;

	JSONValue json;

	string fileName, directoryName;

	private void loadMetadata () {
		metadata.currVersion = json["asset"]["version"].str;
		if ("minVersion" in json["asset"])
			metadata.minVersion = json["asset"]["minVersion"].str;
		if ("generator" in json["asset"])
			metadata.generator = json["asset"]["generator"].str;
		if ("copyright" in json["asset"])
			metadata.copyright = json["asset"]["copyright"].str;
	}

	private void loadScenes () {
		if ("scenes" !in json)
			return;
		scenes.length = json["scenes"].array.length;
		
		if ("scene" in json)
			scene = cast (int) json["scene"].integer;
		foreach (i, scene; json["scenes"].array) {
			if ("name" in scene)
				scenes[i].name = scene["name"].str;
			if ("nodes" in scene) {
				scenes[i].nodes.length = scene["nodes"].array.length;
				foreach (j, node; scene["nodes"].array) {
					scenes[i].nodes[j] = cast (int) node.integer;
				}
			}
		}
	}

	private void loadMeshes () {
		if ("meshes" !in json)
			return;
		meshes.length = json["meshes"].array.length;
		
		foreach (i, mesh; json["meshes"].array) {
			if ("name" in mesh)
				meshes[i].name = mesh["name"].str;
			meshes[i].primitives.length = mesh["primitives"].array.length;
			foreach (j, primitive; mesh["primitives"].array) {
				if ("indices" in primitive)
					meshes[i].primitives[j].indices = cast (int) primitive["indices"].integer;
				if ("material" in primitive)
					meshes[i].primitives[j].material = cast (int) primitive["material"].integer;
				if ("mode" in primitive)
					meshes[i].primitives[j].mode = cast (glTF2Primitive.Mode) primitive["mode"].integer;
				if ("targets" in primitive) {
					meshes[i].primitives[j].targets.length = primitive["targets"].array.length;
					foreach (k, target; primitive["targets"].array) {
						foreach (l, attributeName; attributeNames) {
							if (attributeName in target)
								meshes[i].primitives[j].targets[k][attributeName] = cast (int) target[attributeName].integer;
						}
					}
				}
				foreach (attributeName, attributeIndex; primitive["attributes"].object)
					meshes[i].primitives[j].attributes[attributeName] = cast (uint) attributeIndex.integer;
			}
			if ("weights" in mesh) {
				meshes[i].weights.length = mesh["weights"].array.length;
				foreach (j, weight; mesh["weights"].array)
					meshes[i].weights[j] = getFloatValue (weight);
			}
		}
	}

	private void loadNodes () {
		if ("nodes" !in json)
			return;
		nodes.length = json["nodes"].array.length;
		
		foreach (i, node; json["nodes"].array) {
			if ("name" in node)
				nodes[i].name = node["name"].str;
			if ("camera" in node)
				nodes[i].camera = cast (int) node["camera"].integer;
			if ("children" in node) {
				nodes[i].children.length = node["children"].array.length;
				foreach (j, child; node["children"].array) {
					nodes[i].children[j] = cast (int) child.integer;
				}
			}
			if ("skin" in node)
				nodes[i].skin = cast (int) node["skin"].integer;
			if ("mesh" in node)
				nodes[i].mesh = cast (int) node["mesh"].integer;
			
			if ("translation" in node) {
				enforce (node["translation"].array.length = nodes[i].translation.length, "Node " ~ to!string (i) ~ " translation array length must be " ~ nodes[i].translation.length);
				foreach (j, translationElement; node["translation"].array) {
					nodes[i].translation[j] = getFloatValue (translationElement);
				}
			}
			if ("rotation" in node) {
				enforce (node["rotation"].array.length = nodes[i].rotation.length, "Node " ~ to!string (i) ~ " rotation array length must be " ~ nodes[i].rotation.length);
				foreach (j, rotationElement; node["rotation"].array) {
					nodes[i].rotation[j] = getFloatValue (rotationElement);
				}
			}
			if ("scale" in node) {
				enforce (node["scale"].array.length = nodes[i].scale.length, "Node " ~ to!string (i) ~ " scale array length must be " ~ nodes[i].scale.length);
				foreach (j, scaleElement; node["scale"].array) {
					nodes[i].scale[j] = getFloatValue (scaleElement);
				}
			}
			if ("matrix" in node) {
				enforce (node["matrix"].array.length = nodes[i].matrix.length, "Node " ~ to!string (i) ~ " matrix array length must be " ~ nodes[i].matrix.length);
				foreach (j, matrixElement; node["matrix"].array) {
					nodes[i].matrix[j] = getFloatValue (matrixElement);
				}			
			}
			if ("weights" in node) {
				nodes[i].weights.length = node["weights"].array.length;
				foreach (j, weight; node["weights"].array) {
					nodes[i].weights[j] = getFloatValue (weight);
				}
			}
		}
	}

	private void loadSkins () {
		if ("skins" !in json)
			return;
		skins.length = json["skins"].array.length;
		
		foreach (i, skin; json["skins"].array) {
			skins[i].joints.length = skin["joints"].array.length;
			foreach (j, joint; skin["joints"].array) {
				skins[i].joints[j] = cast (int) joint.integer;
			}
			if ("skeleton" in skin)
				skins[i].skeleton = cast (int) skin["skeleton"].integer;
			if ("inverseBindMatrices" in skin)
				skins[i].inverseBindMatrices = cast (int) skin["inverseBindMatrices"].integer;
		}
	}

	private void loadAnimations () {
		if ("animations" !in json)
			return;
		animations.length = json["animations"].array.length;
		
		foreach (i, animation; json["animations"].array) {
			if ("name" in animation) {
				animations[i].name = animation["name"].str;
			}
			animations[i].samplers.length = animation["samplers"].array.length;

			foreach (j, sampler; animation["samplers"].array) {
				animations[i].samplers[j].input = cast (int) sampler["input"].integer;
				animations[i].samplers[j].output = cast (int) sampler["output"].integer;
				if ("interpolation" in sampler) {
					foreach (k, interpolationMode; interpolationModes) {
						if (interpolationMode == sampler["interpolation"].str)
							animations[i].samplers[j].interpolation = cast (glTF2Animation.Interpolation) k;
					}
				}
			}

			animations[i].channels.length = animation["channels"].array.length;
			foreach (j, channel; animation["channels"].array) {
				animations[i].channels[j].sampler = cast (int) channel["sampler"].integer;
				if ("node" in channel["target"])
					animations[i].channels[j].target.node = cast (int) channel["target"]["node"].integer;
				foreach (k, targetType; targetTypes) {
					if (targetType == channel["target"]["path"].str)
						animations[i].channels[j].target.path = cast (glTF2Animation.Path) k;
				}
			}
		}
	}

	private void loadBuffers () {
		if ("buffers" !in json)
			return;
		buffers.length = json["buffers"].array.length;
		
		foreach (i, buffer; json["buffers"].array) {
			if ("name" in buffer)
				buffers[i].name = buffer["name"].str;
			buffers[i].byteLength = cast (int) buffer["byteLength"].integer;
			if ("uri" in buffer)
				buffers[i].uri = buffer["uri"].str;
		}
	}

	private void loadBufferViews () {
		if ("bufferViews" !in json)
			return;
		bufferViews.length = json["bufferViews"].array.length;
		
		foreach (i, bufferView; json["bufferViews"].array) {
			if ("name" in bufferView)
				bufferViews[i].name = bufferView["name"].str;
			bufferViews[i].buffer = cast (int) bufferView["buffer"].integer;
			if ("byteOffset" in bufferView)
				bufferViews[i].byteOffset = bufferView["byteOffset"].integer;
			bufferViews[i].byteLength = bufferView["byteLength"].integer;
			if ("byteStride" in bufferView)
				bufferViews[i].byteStride = cast (int) bufferView["byteStride"].integer;
			if ("target" in bufferView)
				bufferViews[i].target = cast (glTF2BufferView.TargetType) bufferView["target"].integer;
			
		}
	}

	private void loadAccessors () {
		if ("accessors" !in json)
			return;
		accessors.length = json["accessors"].array.length;
		
		foreach (i, accessor; json["accessors"].array) {
			accessors[i].componentType = cast (glTF2Accessor.ComponentType) accessor["componentType"].integer;
			foreach (j, typeName; typeNames) {
				if (typeName == accessor["type"].str)
					accessors[i].type = cast (glTF2Accessor.Type) j;
			}
			accessors[i].bufferView = cast (int) accessor["bufferView"].integer;
			if ("byteOffset" in accessor)
				accessors[i].byteOffset = accessor["byteOffset"].integer;
			if ("normalized" in accessor) {
				if (accessor["normalized"].type == JSON_TYPE.TRUE)
					accessors[i].normalized = true;
				else
					accessors[i].normalized = false;
			}
			accessors[i].count = accessor["count"].integer;
			int minMaxArrayLength = 0;
			if ("min" in accessor) {
				switch (accessors[i].type) {
					case glTF2Accessor.Type.Scalar: minMaxArrayLength = 1; break;
					case glTF2Accessor.Type.Vec2: minMaxArrayLength = 2; break;
					case glTF2Accessor.Type.Vec3: minMaxArrayLength = 3; break;
					case glTF2Accessor.Type.Vec4: minMaxArrayLength = 4; break;
					case glTF2Accessor.Type.Mat2: minMaxArrayLength = 4; break;
					case glTF2Accessor.Type.Mat3: minMaxArrayLength = 9; break;
					case glTF2Accessor.Type.Mat4: minMaxArrayLength = 16; break;
					default: break;
				}
				if (accessors[i].componentType == glTF2Accessor.ComponentType.Float) {
					accessors[i].min.f.length = minMaxArrayLength;
					foreach (j, minElement; accessor["min"].array)
						accessors[i].min.f[j] = getFloatValue (minElement);
				}
				else {
					accessors[i].min.i.length = minMaxArrayLength;
					foreach (j, minElement; accessor["min"].array)
						accessors[i].min.i[j] = cast (int) minElement.integer;
				}
			}
			if ("max" in accessor) {
				switch (accessors[i].type) {
					case glTF2Accessor.Type.Scalar: minMaxArrayLength = 1; break;
					case glTF2Accessor.Type.Vec2: minMaxArrayLength = 2; break;
					case glTF2Accessor.Type.Vec3: minMaxArrayLength = 3; break;
					case glTF2Accessor.Type.Vec4: minMaxArrayLength = 4; break;
					case glTF2Accessor.Type.Mat2: minMaxArrayLength = 4; break;
					case glTF2Accessor.Type.Mat3: minMaxArrayLength = 9; break;
					case glTF2Accessor.Type.Mat4: minMaxArrayLength = 16; break;
					default: break;
				}
				if (accessors[i].componentType == glTF2Accessor.ComponentType.Float) {
					accessors[i].max.f.length = minMaxArrayLength;
					foreach (j, maxElement; accessor["max"].array)
						accessors[i].max.f[j] = getFloatValue (maxElement);
				}
				else {
					accessors[i].max.i.length = minMaxArrayLength;
					foreach (j, maxElement; accessor["max"].array)
						accessors[i].max.i[j] = cast (int) maxElement.integer;
				}
			}
			if ("sparse" in accessor) {
				accessors[i].sparse.count = cast (int) accessor["sparse"]["count"].integer;

				accessors[i].sparse.indices.bufferView = cast (int) accessor["sparse"]["indices"]["bufferView"].integer;
				accessors[i].sparse.indices.componentType = cast (glTF2Accessor.ComponentType) accessor["sparse"]["indices"]["componentType"].integer;
				if ("byteOffset" in accessor["sparse"]["indices"])
					accessors[i].sparse.indices.byteOffset = cast (int) accessor["sparse"]["indices"]["byteOffset"].integer;

				accessors[i].sparse.values.bufferView = cast (int) accessor["sparse"]["values"]["bufferView"].integer;
				if ("byteOffset" in accessor["sparse"]["values"])
					accessors[i].sparse.values.byteOffset = cast (int) accessor["sparse"]["values"]["byteOffset"].integer;
			}
		}
	}

	private void loadMaterials () {
		if ("materials" !in json)
			return;
		materials.length = json["materials"].array.length;
		
		foreach (i, material; json["materials"].array) {
			if ("name" in material)
				materials[i].name = material["name"].str;
			if ("pbrMetallicRoughness" in material) {
				if ("baseColorFactor" in material["pbrMetallicRoughness"]) {
					enforce (material["pbrMetallicRoughness"]["baseColorFactor"].array.length = materials[i].pbrMetallicRoughness.baseColorFactor.length, "material " ~ to!string (i) ~ " baseColorFactor array length must be " ~ materials[i].pbrMetallicRoughness.baseColorFactor.length);
					foreach (j, colorComponent; material["pbrMetallicRoughness"]["baseColorFactor"].array) {
						materials[i].pbrMetallicRoughness.baseColorFactor[j] = getFloatValue (colorComponent);
					}
				}
				if ("baseColorTexture" in material["pbrMetallicRoughness"]) {
					materials[i].pbrMetallicRoughness.baseColorTexture.index = cast (int) material["pbrMetallicRoughness"]["baseColorTexture"]["index"].integer;
					if ("texCoord" in material["pbrMetallicRoughness"]["baseColorTexture"])
						materials[i].pbrMetallicRoughness.baseColorTexture.texCoord = cast (int) material["pbrMetallicRoughness"]["baseColorTexture"]["texCoord"].integer;
				}
				if ("metallicFactor" in material["pbrMetallicRoughness"])
					materials[i].pbrMetallicRoughness.metallicFactor = getFloatValue (material["pbrMetallicRoughness"]["metallicFactor"]);
				if ("roughnessFactor" in material["pbrMetallicRoughness"])
					materials[i].pbrMetallicRoughness.roughnessFactor = getFloatValue (material["pbrMetallicRoughness"]["roughnessFactor"]);
				if ("metallicRoughnessTexture" in material["pbrMetallicRoughness"]) {
					materials[i].pbrMetallicRoughness.metallicRoughnessTexture.index = cast (int) material["pbrMetallicRoughness"]["metallicRoughnessTexture"]["index"].integer;
					if ("texCoord" in material["pbrMetallicRoughness"]["metallicRoughnessTexture"])
						materials[i].pbrMetallicRoughness.metallicRoughnessTexture.texCoord = cast (int) material["pbrMetallicRoughness"]["metallicRoughnessTexture"]["texCoord"].integer;
				}
			}
			if ("normalTexture" in material) {
				materials[i].normalTexture.index = cast (int) material["normalTexture"]["index"].integer;
				if ("texCoord" in material["normalTexture"])
					materials[i].normalTexture.texCoord = cast (int) material["normalTexture"]["texCoord"].integer;
				if ("scale" in material["normalTexture"])
					materials[i].normalTexture.scale = getFloatValue (material["normalTexture"]["scale"]);
			}
			if ("occlusionTexture" in material) {
				materials[i].occlusionTexture.index = cast (int) material["occlusionTexture"]["index"].integer;
				if ("texCoord" in material["occlusionTexture"])
					materials[i].occlusionTexture.texCoord = cast (int) material["occlusionTexture"]["texCoord"].integer;
				if ("strength" in material["occlusionTexture"])
					materials[i].occlusionTexture.strength = getFloatValue (material["occlusionTexture"]["strength"]);
			}
			if ("emissiveTexture" in material) {
				materials[i].emissiveTexture.index = cast (int) material["emissiveTexture"]["index"].integer;
				if ("texCoord" in material["emissiveTexture"])
					materials[i].emissiveTexture.texCoord = cast (int) material["emissiveTexture"]["texCoord"].integer;
			}
			if ("emissiveFactor" in material) {
				enforce (material["emissiveFactor"].array.length = materials[i].emissiveFactor.length, "material " ~ to!string (i) ~ " emissiveFactor array length must be " ~ materials[i].emissiveFactor.length);
				foreach (j, colorComponent; material["emissiveFactor"].array) 
					materials[i].emissiveFactor[j] = getFloatValue (colorComponent);
				
			}
			if ("alphaMode" in material)
				foreach (j, alphaModeName; alphaModes) {
					if (alphaModeName == material["alphaMode"].str)
						materials[i].alphaMode = cast (glTF2Material.AlphaMode) j;
				}
			if ("alphaCutoff" in material)
				materials[i].alphaCutoff = getFloatValue (material["alphaCutoff"]);
			if ("doubleSided" in material) {
				if (material["doubleSided"].type == JSON_TYPE.TRUE)
					materials[i].doubleSided = true;
				else
					materials[i].doubleSided = false;
			}
		}
	}

	private void loadCameras () {
		if ("cameras" !in json)
			return;
		cameras.length = json["cameras"].array.length;
		
		foreach (i, camera; json["cameras"].array) {
			enforce (((camera["type"].str == "perspective") || (camera["type"].str == "orthographic")), "Camera " ~ to!string(i) ~ " - camera type should be 'perspective' or 'orthographic'");
			if (camera["type"].str == "perspective") {
				cameras[i].cameraType = glTF2Camera.CameraType.Perspective;
				cameras[i].znear = getFloatValue (camera["perspective"]["znear"]);
				cameras[i].yfov = getFloatValue (camera["perspective"]["yfov"]);
				if ("zfar" in camera["perspective"])
					cameras[i].zfar = getFloatValue (camera["perspective"]["zfar"]);
				if ("aspectRatio" in camera["perspective"])
					cameras[i].aspectRatio = getFloatValue (camera["perspective"]["aspectRatio"]);
			}
			else {
				cameras[i].cameraType = glTF2Camera.CameraType.Orthographic;
				cameras[i].znear = getFloatValue (camera["orthographic"]["znear"]);
				cameras[i].zfar = getFloatValue (camera["orthographic"]["zfar"]);
				cameras[i].xmag = getFloatValue (camera["orthographic"]["xmag"]);
				cameras[i].ymag = getFloatValue (camera["orthographic"]["ymag"]);
			}
		}
	}

	private void loadImages () {
		if ("images" !in json)
			return;
		images.length = json["images"].array.length;
		
		foreach (i, image; json["images"].array) {
			if ("name" in image)
				images[i].name = image["name"].str;		
			if ("mimeType" in image)
				images[i].mimeType = image["mimeType"].str;
			if ("bufferView" in image)
				images[i].bufferView = cast (int) image["bufferView"].integer;
			if ("uri" in image)
				images[i].uri = image["uri"].str;
		}
	}

	private void loadSamplers () {
		if ("samplers" !in json)
			return;
		samplers.length = json["samplers"].array.length;
		
		foreach (i, sampler; json["samplers"].array) {
			if ("name" in sampler)
				samplers[i].name = sampler["name"].str;
			if ("magFilter" in sampler)
				samplers[i].magFilter = cast (glTF2Sampler.MagFilter) sampler["magFilter"].integer;
			if ("minFilter" in sampler)
				samplers[i].minFilter = cast (glTF2Sampler.MinFilter) sampler["magFilter"].integer;
			if ("wrapS" in sampler)
				samplers[i].wrapS = cast (glTF2Sampler.WrappingMode) sampler["wrapS"].integer;
			if ("wrapT" in sampler)
				samplers[i].wrapT = cast (glTF2Sampler.WrappingMode) sampler["wrapT"].integer;
		}
	}

	private void loadTextures () {
		if ("textures" !in json)
			return;
		textures.length = json["textures"].array.length;
		
		foreach (i, texture; json["textures"].array) {
			if ("name" in texture)
				textures[i].name = texture["name"].str;
			if ("sampler" in texture)
				textures[i].sampler = cast (int) texture["sampler"].integer;
			if ("source" in texture)
				textures[i].source = cast (int) texture["source"].integer;
		}
	}

	private JSONValue readBinaryBlob (ubyte[] fileContent) {
		uint binaryContainerVersion = fileContent.peek!(uint, Endian.littleEndian) (4);
		enforce (binaryContainerVersion == 2, "Version " ~ binaryContainerVersion.to!string ~ " of the binary container is declared. Only version 2 is supported.");
		uint totalLength = fileContent.peek!(uint, Endian.littleEndian) (8);
		enforce (totalLength == fileContent.length, "Declared file length does not match actual file length.");

		char[] jsonChunkIdentifier = cast (char[]) fileContent [16 .. 20];
		enforce (jsonChunkIdentifier == "JSON", "JSON chunk not found.");
		uint jsonChunkLength = fileContent.peek!(uint, Endian.littleEndian) (12);
		string jsonChunkData = cast (string) fileContent [20 .. 20 + jsonChunkLength];

		JSONValue json = parseJSON (jsonChunkData);

		return json;
	}

	void loadBufferData (ubyte[] fileContent) {
		foreach (i, ref buffer; buffers) {
			if (buffer.uri) {
				if (buffer.uri.length > 4 && buffer.uri[0 .. 5] == "data:") {
					auto splits = findSplitAfter (buffer.uri, "base64,");
					if (splits[1].length % 4 == 2)
						splits[1] ~= "==";
					if (splits[1].length % 4 == 3)
						splits[1] ~= "=";
					buffer.data = Base64.decode (splits[1]);
				}
				else {
					buffer.data = cast (ubyte[]) read (directoryName ~ "/" ~ buffer.uri);
				}
			}
			else {
				uint totalLength = fileContent.peek!(uint, Endian.littleEndian) (8);
				uint jsonChunkLength = fileContent.peek!(uint, Endian.littleEndian) (12);
				uint binaryChunkOffset = jsonChunkLength + 20;
				enforce (totalLength > binaryChunkOffset, "Unexpected end of file.");

				char[4] binaryChunkIdentifier = cast (char[]) fileContent [binaryChunkOffset + 4.. binaryChunkOffset + 8];
				enforce (binaryChunkIdentifier == "BIN\0", "Binary chunk not found.");

				uint binaryChunkLength = fileContent.peek!(uint, Endian.littleEndian) (binaryChunkOffset);
				void[] data = cast (void[]) fileContent [binaryChunkOffset + 8 .. binaryChunkOffset + 8 + binaryChunkLength];

				buffer.data = data;
			}
		}
	}

	void loadImageData () {
		foreach (i, ref image; images) {
			if (image.bufferView == -1) {
				if (image.uri.length > 4 && image.uri[0 .. 5] == "data:") {
					auto splits = findSplit (image.uri[5 .. $], ";base64,");

					image.mimeType = splits[0];
					if (splits[2].length % 4 == 2)
						splits[2] ~= "==";
					if (splits[2].length % 4 == 3)
						splits[2] ~= "=";

					image.data = Base64.decode (splits[2]);
				}
				else {
					image.data = read (directoryName ~ "/" ~ image.uri);
					enforce (image.data, "Failed to load " ~ directoryName ~ "/" ~ image.uri);
				}
			}
			//NB: image data already in buffers is not duplicated; reading image.data is NOT recommended, use asset.accessImage (imageIndex) instead
		}
	}

	private void resetAsset () {
		json = null;
		fileName = null;
		directoryName = null;

    	scene = -1;
		metadata = cast (glTF2Metadata) null;
		scenes = null;
		meshes = null;
		nodes = null;
		skins = null;
		animations = null;
		buffers = null;
		bufferViews = null;
		accessors = null;
		materials = null;
		cameras = null;
		images = null;
		samplers = null;
		textures = null;
	}

	void load (string fileName) {
		resetAsset ();

		this.fileName = fileName;
		this.directoryName = dirName (fileName);
		ubyte[] fileContent = cast (ubyte[]) read (fileName);
		enforce (fileContent, "Failed to load " ~ fileName);

		if (cast (char[]) fileContent[0 .. 4] == "glTF") {
			json = readBinaryBlob (fileContent);
		}
		else
			json = parseJSON (cast (string) fileContent);
		
		if ("extensionsRequired" in json) {
			writeln ("No extensions are supported at this time. Aborting.");
			return;
		}

		loadMetadata ();
		loadScenes ();
		loadMeshes ();
		loadNodes ();
		loadSkins ();
		loadAnimations ();
		loadBuffers ();
		loadBufferViews ();
		loadAccessors ();
		loadMaterials ();
		loadCameras ();
		loadImages ();
		loadSamplers ();
		loadTextures ();

		loadBufferData (fileContent);
		loadImageData ();
	}

	this (string fileName) {
		load (fileName);
	}

	void[] accessData (uint accessor) {
		ulong offset = accessors[accessor].byteOffset + bufferViews [accessors[accessor].bufferView].byteOffset;

		uint elementSize;
		final switch (accessors[accessor].componentType) {
			case glTF2Accessor.ComponentType.Byte, glTF2Accessor.ComponentType.UnsignedByte: elementSize = 1; break;
			case glTF2Accessor.ComponentType.Short, glTF2Accessor.ComponentType.UnsignedShort: elementSize = 2; break;
			case glTF2Accessor.ComponentType.UnsignedInt, glTF2Accessor.ComponentType.Float: elementSize = 4; break;
		}
		final switch (accessors[accessor].type) {
			case glTF2Accessor.Type.Scalar: break;
			case glTF2Accessor.Type.Vec2: elementSize *= 2; break;
			case glTF2Accessor.Type.Vec3: elementSize *= 3; break;
			case glTF2Accessor.Type.Vec4, glTF2Accessor.Type.Mat2: elementSize *= 4; break;
			case glTF2Accessor.Type.Mat3: elementSize *= 9; break;
			case glTF2Accessor.Type.Mat4: elementSize *= 16; break;
		}
		ulong elementCount = accessors[accessor].count;
		ulong byteLength = elementCount * elementSize;

		void[] data = buffers[bufferViews[accessors[accessor].bufferView].buffer].data [offset .. offset + byteLength];

		return data;
	}

	void[][uint] accessSparseData (uint sparseAccessor) {
		enforce (accessors[sparseAccessor].sparse.count > 0, "No sparse accessor found at accessor index " ~ sparseAccessor.to!string ~ ".");

		void[][uint] data;

		//retrieving values
		uint dataElementSize;
		final switch (accessors[sparseAccessor].componentType) {
			case glTF2Accessor.ComponentType.Byte, glTF2Accessor.ComponentType.UnsignedByte: dataElementSize = 1; break;
			case glTF2Accessor.ComponentType.Short, glTF2Accessor.ComponentType.UnsignedShort: dataElementSize = 2; break;
			case glTF2Accessor.ComponentType.UnsignedInt, glTF2Accessor.ComponentType.Float: dataElementSize = 4; break;
		}
		final switch (accessors[sparseAccessor].type) {
			case glTF2Accessor.Type.Scalar: break;
			case glTF2Accessor.Type.Vec2: dataElementSize *= 2; break;
			case glTF2Accessor.Type.Vec3: dataElementSize *= 3; break;
			case glTF2Accessor.Type.Vec4, glTF2Accessor.Type.Mat2: dataElementSize *= 4; break;
			case glTF2Accessor.Type.Mat3: dataElementSize *= 9; break;
			case glTF2Accessor.Type.Mat4: dataElementSize *= 16; break;
		}
		ulong dataByteLength = accessors[sparseAccessor].sparse.count * dataElementSize;
		ulong dataOffset = accessors[sparseAccessor].sparse.values.byteOffset + bufferViews[accessors[sparseAccessor].sparse.values.bufferView].byteOffset;
		void[] rawData = cast (void[]) buffers[bufferViews[accessors[sparseAccessor].sparse.values.bufferView].buffer].data [dataOffset .. dataOffset + dataByteLength];

		//retrieving indices
		uint indicesElementSize;
		switch (accessors[sparseAccessor].sparse.indices.componentType) {
			case glTF2Accessor.ComponentType.UnsignedByte: indicesElementSize = 1; break;
			case glTF2Accessor.ComponentType.UnsignedShort: indicesElementSize = 2; break;
			case glTF2Accessor.ComponentType.UnsignedInt: indicesElementSize = 4; break;
			default: (enforce (0, "Wrong sparse accessor indices type.")); break;
		}
		ulong indicesByteLength = accessors[sparseAccessor].sparse.count * indicesElementSize;
		ulong indicesOffset = accessors[sparseAccessor].sparse.indices.byteOffset + bufferViews[accessors[sparseAccessor].sparse.indices.bufferView].byteOffset;
		void[] rawIndices = buffers[bufferViews[accessors[sparseAccessor].sparse.indices.bufferView].buffer].data [indicesOffset .. indicesOffset + indicesByteLength];

		//populating the array
		for (int i = 0; i < accessors[sparseAccessor].sparse.count; i++) {
			switch (accessors[sparseAccessor].sparse.indices.componentType) {
				case glTF2Accessor.ComponentType.UnsignedByte:
					ubyte index = (cast (ubyte[]) rawIndices).peek!(ubyte, Endian.littleEndian) (ubyte.sizeof * i);
					data[index] = cast (void[]) rawData [dataElementSize * i .. dataElementSize * (i + 1)];
				break;
				case glTF2Accessor.ComponentType.UnsignedShort:
					ushort index = (cast (ubyte[]) rawIndices).peek!(ushort, Endian.littleEndian) (ushort.sizeof * i);
					data[index] = cast (void[]) rawData [dataElementSize * i .. dataElementSize * (i + 1)];
				break;
				case glTF2Accessor.ComponentType.UnsignedInt:
					uint index = (cast (ubyte[]) rawIndices).peek!(uint, Endian.littleEndian) (uint.sizeof * i);
					data[index] = cast (void[]) rawData [dataElementSize * i .. dataElementSize * (i + 1)];
				break;
			default: break;
			}
		}

		return data;
	}

	void[] accessImage (uint imageIndex) {
		if (images[imageIndex].bufferView != -1) {
			uint bufferView = images[imageIndex].bufferView;
			ulong offset = bufferViews[bufferView].byteOffset;
			ulong byteLength = bufferViews[bufferView].byteLength;
			return buffers[bufferViews[bufferView].buffer].data [offset .. offset + byteLength];
		}
		else
			return images[imageIndex].data;
	}
};