import os
import json
import random

schemas = [
    {"type": "integer"},
    {"type": "number"},
    {"type": "boolean"},
    {
        "enum": ["android", "ios"],
        "type": "string"
    },
    {
        "pattern": ".*",
        "type": "string"
    },
    {"type": "object"}
]

def generate_value(schema):
    t = schema.get("type")

    if t == "integer":
        return random.randint(1, 100)

    if t == "number":
        return random.random() * 100

    if t == "boolean":
        return random.choice([True, False])

    if t == "string":
        if "enum" in schema:
            return random.choice(schema["enum"])
        return "sample_string"

    if t == "object":
        return {"example": "value"}

    # fallback
    return None


def generate_configs(n=300, skip=0):
    configs = []

    for i in range(skip + 1, skip + n + 1):
        schema = random.choice(schemas)
        cfg = {
            "key": f"config_{i}",
            "value": generate_value(schema),
            "schema": schema,
            "description": f"Auto-generated config {i}",
            "changeReason": "Initial creation",
            "functionName": None,
            "autocompleteFunctionName": None
        }
        configs.append(cfg)

    return configs


def load_existing_configs(path):
    if not os.path.exists(path):
        return []
    with open(path, "r") as f:
        return json.load(f)

def save_configs(path, configs):
    with open(path, "w") as f:
        json.dump(configs, f, indent=2)

if __name__ == "__main__":
    filename = "configs.json"
    # existing_configs = load_existing_configs(filename)
    n = 2000
    new_configs = generate_configs(n)
    save_configs(filename, new_configs)

    print(f"Generated configs.json with {n} default configs")

