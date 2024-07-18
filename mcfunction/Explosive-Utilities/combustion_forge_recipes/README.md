# Combustion Forge Recipe Generation

The recipes for the combustion forge are written in JSON files with a similar format to vanilla recipes.

However, these files on their own are useless, and must be compiled into actual code inside function files to operate.

In order to run the compliation script, you will need [Python 3](https://www.python.org "Python's website") and [jsonschema](https://pypi.org/project/jsonschema "jsonschema on PyPI") installed. You can install jsonschema from pip using the following command:

```console
pip install jsonschema
```

See the following sections for the recipe file format and how to add them to a project.

## Recipe JSON File Format

 - {object} The root tag.
   - {string} **\$schema** - technically, setting **\$schema** in a json file marks it as a JSON schema, rather than a data file. However, if you set this key to the relative path from this recipe file to the recipe schema, you will get autocompletion and linting for the recipe without having to configure your development enviroment, assuming it is capable enough with JSON. Rules are meant to be broken!
   - {string} **type**: `"combustion_forge_shaped"` for shaped recipes, or `"combustion_forge_shapeless"` for shapeless.
   - {object or object list} **result**: either a single result or a list of results. The output items of the recipe.
     - {string} **type**: either `"item"` or `"loot_table"`.
     - If **type** is `"item"`: {string} **item**: the item ID. Must include namespace.
     - If **type** is `"item"`: {optional, integer} **count**: the amount of the item to make. Defaults to 1.
     - If **type** is `"item"`: {optional, string} **nbt**: the nbt data to go in the item's tag nbt field. Example: `"{CustomModelData:1385753,display:{Name:'{\"text\":\"Reactive Plating\",\"italic\":false,\"color\":\"red\"}',Lore:['{\"text\":\"Explosive Utilities\",\"italic\":false,\"color\":\"blue\"}']}}"`
     - If **type** is `"loot_table"`: {string} **loot**: the loot table ID. Must include namespace.
   - If **type** is `"combustion_forge_shaped"`: {list} **pattern**: a list of single character keys used to describe a pattern for shaped recipes. Each row in the crafting grid is one string in this list containing 3 or less keys. All strings in this list need to have the same amount of keys. A space can be used to indicate an empty spot. All non-space characters must be present in **key**.
   - If **type** is  `"combustion_forge_shaped"`: {object} **key**: All keys used for this shaped crafting recipe. 
     - {object or object list} (A key): The ingredients or a list of ingredients corresponding to this key.
       - {string or string list} **item**: an item ID or list of item IDs. Must include namespace.
       - {optional, string} **nbt**: the nbt data in the item's tag nbt field. Example: `"{CustomModelData:1385753,display:{Name:'{\"text\":\"Reactive Plating\",\"italic\":false,\"color\":\"red\"}',Lore:['{\"text\":\"Explosive Utilities\",\"italic\":false,\"color\":\"blue\"}']}}"`
   - If **type** is  `"combustion_forge_shapeless"`: {list} **ingredients**: A list of entries for this shapeless crafting recipe. The total count of the ingredients matched from each entry must be at least 1 and no greater than 9. Each entry must match to a unique item, no overlap.
     - {object or object list}: An entry made of a single item or item list to match for the ingredient. If using a list, **count** in each item must be equal. 
       - {string or string list} **item**: an item ID or list of item IDs. Must include namespace.
       - {optional, integer} **count**: the number of times the item must appear in the crafting grid. Defaults to 1.
       - {optional, string} **nbt**: the nbt data in the item's tag nbt field. Example: `"{CustomModelData:1385753,display:{Name:'{\"text\":\"Reactive Plating\",\"italic\":false,\"color\":\"red\"}',Lore:['{\"text\":\"Explosive Utilities\",\"italic\":false,\"color\":\"blue\"}']}}"`

## Adding Recipes To Explosive Utilites

To add recipes to Explosive Utilites, place the recipe files inside `{PROJECT_ROOT}/combustion_forge_recipes/recipes` within the relavent subdirectory.

Then, in the root directory of the project, run one the following command:

```console
python3 combustion_forge_recipes/compile_recipes.py
``` 

## Adding Recipes Through Your Own Datapack

To add recipes to the combustion forge through your own datapack, firstly, create a directory in your project to store the recipes and compiler in, for this example we will call it `combustion_forge_recipes`. Then, copy over [compile_recipes.py](compile_recipes.py) and [recipe_schema.json](recipe_schema.json) into `combustion_forge_recipes`. Place your custom recipes within either that directory, or a subdirectory within.

Next, you will need to change some of the config variables in the `compile_recipes.py` to suit your project, namely:
- **combustion_forge_recipe_directory** - set this to be the relative path from the root directory of your project to the directory containing your custom recipes.
- **recipe_output_directory** - set this to be the relative path from `combustion_forge_recipe_directory` to the directory in your datapack that you would like the compiled recipe function files to go. For example, *../Example-DP/data/exampledatapack/functions/generated/combustion_forge_recipes*.
- **recipe_schema_file_path** - set this to be the relative file path from `combustion_forge_recipe_directory` to the recipe schema file you copied earlier.
- **recipe_function_tag_file_path** - the compiler will generate a function tag that contains all the generated recipe functions. set this to be the relative file path from `combustion_forge_recipe_directory` to the file you would like to to output to. For example, *../Example-DP/data/exampledatapack/tags/functions/generated/combustion_forge_recipes.json*.
- **recipe_function_directory_id** - set this to be the function directory id that the recipe function files are being outputted to. Going with the example for `recipe_output_directory`, this would be *exampledatapack:generated/combustion_forge_recipes/*.

Additionally, you might want to temporarily set **logging_level** to `logging.NOTSET` to check if your recipes are being compiled and outputted correctly. 

To run, execute `compile_recipes.py` from the root directory of your project.

Once you've ensured that is all working correctly, you will need to add the generated function tag with the recipe functions into Explosive Utilities' main recipe function tag. To do so, create the file *xplsvtlts/tags/functions/combustion_forge_recipes.json* in the data directory of your datapack (going with the previous examples, the full path from the project root would be *Example-DP/data/xplsvtlts/tags/functions/combustion_forge_recipes.json*.) In this function tag file, add the function id tag with the generate recipes. The file should look something like this:

```json
{
    "values": [
        "#exampledatapack:generated/combustion_forge_recipes"
    ],
    "replace": false
}
```

Make sure to set **replace** to `false` (`false` is default but better safe than sorry) to not override the recipes from Explosive Utilities and other datapacks. If you wish to override the recipes from Explosive Utilities, set **values** to an empty list and **replace** to `true` in  *xplsvtlts/tags/functions/generated/combustion_forge_recipes.json* within the data directory of your datapack.

Afterwards, reload and test your recipes in the combustion forge. If they don't work, I wish you luck in debugging hell ;).