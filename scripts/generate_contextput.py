import json
import itertools
import random

# Number of context overrides to generate per default config
CONTEXTS_PER_CONFIG = 1000   # <--- CHANGE THIS ANYTIME YOU WANT

CONTEXT_VALUES = {
    "Tenant": [ "tenant1", "tenant2", "tenant3", "tenant4", "tenant5", "tenant6", "tenant7", "tenant8", "tenant9", "tenant10"],
    "Reseller": [ "reseller1", "reseller2", "reseller3", "reseller4", "reseller5","reseller6","reseller7","reseller8","reseller9","reseller10", ],
    "Merchant": [ "merchant1", "merchant2", "merchant3", "merchant4", "merchant5", "merchant6", "merchant7", "merchant8", "merchant9", "merchant10" ],
    "Sub_Merchant_1": [ "subm1", "subm2", "subm3", "subm4", "subm5", "subm6", "subm7", "subm8", "subm9", "subm10"],
    "Sub_Merchant_2": [ "subm11", "subm12", "subm13", "subm14", "subm15", "subm16", "subm17", "subm18", "subm19", "subm110",]
}

CONTEXT_COMBINATIONS = list(itertools.combinations(CONTEXT_VALUES.keys(), 1)) + list(itertools.combinations(CONTEXT_VALUES.keys(), 2)) + list(itertools.combinations(CONTEXT_VALUES.keys(), 3)) 

random.shuffle(CONTEXT_COMBINATIONS)


def build_context(combo_keys, autogen_index):
    return {"Tenant" : f"tenant_{autogen_index}"}

def random_object():
    return {
        "x": random.choice([1, 2, 3]),
        "y": random.choice(["a", "b", "c"]),
        "flag": random.choice([True, False])
    }


def generate_value_from_schema(key, schema, autogen_index):

    # If schema is NOT an object / dict, skip
    if not isinstance(schema, dict):
        return None

    t = schema.get("type")

    if "enum" in schema:
        return schema["enum"][ (autogen_index - 1) % len(schema["enum"]) ]

    if t == "integer":
        return autogen_index + 10

    if t == "number":
        return float(autogen_index) + 0.5

    if t == "boolean":
        return (autogen_index % 2 == 1)

    if t == "string":
        return f"{key}_val_{autogen_index}"

    if t == "object":
        return random_object()

    return None


def generate_override(config, autogen_index):
    override_obj = {}
    key = config["key"]
    schema = config["schema"]
    override = generate_value_from_schema( key, schema, autogen_index )
    override_obj[key] = override

    return override_obj


def generate_contextput(config, default_config_index, i):

    # Start indexing from 1:
    # Example for config 0: indices → 1, 2, 3
    # Example for config 1: indices → 4, 5, 6
    autogen_index = default_config_index * CONTEXTS_PER_CONFIG + (i + 1)

    combo_keys = CONTEXT_COMBINATIONS[autogen_index % len(CONTEXT_COMBINATIONS)]
    ctx = build_context(combo_keys, i+1)

    return {
        "context": ctx,
        "override": generate_override(config, autogen_index),
        "description": f"Auto-generated {autogen_index}",
        "change_reason": "sync"
    }

def load_configs(filename): 
    with open(filename) as f:
        configs = json.load(f)
        return configs
    

def main():

    configs = load_configs("rand.json")

    results = []

    for default_config_index, item in enumerate(configs):
        for i in range(CONTEXTS_PER_CONFIG):
            v = generate_contextput(item, default_config_index, i)
            results.append(v)

    with open("generated_contextputs.json", "w") as f:
        json.dump(results, f, indent=2)

    print("✔ generated_contextputs.json created")


if __name__ == "__main__":
    main()

