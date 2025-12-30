import json
import random

CONTEXTS_PER_CONFIG = 500

def build_context(i):
    # i starts from 1
    return {
        "Tenant": f"tenant_{i}"
    }


def random_object():
    return {
        "x": random.choice([1, 2, 3]),
        "y": random.choice(["a", "b", "c"]),
        "flag": random.choice([True, False])
    }


def generate_value_from_schema(key, schema, autogen_index):
    if not isinstance(schema, dict):
        return None

    t = schema.get("type")

    if "enum" in schema:
        return schema["enum"][(autogen_index - 1) % len(schema["enum"])]

    if t == "integer":
        return autogen_index + 10

    if t == "number":
        return float(autogen_index) + 0.5

    if t == "boolean":
        return autogen_index % 2 == 1

    if t == "string":
        return f"{key}_val_{autogen_index}"

    if t == "object":
        return random_object()

    return None


def build_override_for_all_configs(configs, autogen_index):
    """
    Builds:
    {
      config_1: value,
      config_2: value,
      ...
    }
    """
    override = {}

    for config in configs:
        key = config["key"]
        schema = config["schema"]
        override[key] = generate_value_from_schema(key, schema, autogen_index)

    return override


def load_configs(filename):
    with open(filename) as f:
        return json.load(f)


def main():
    configs = load_configs("configs.json")
    results = []

    for i in range(1, CONTEXTS_PER_CONFIG + 1):
        result = {
            "context": build_context(i),
            "override": build_override_for_all_configs(configs, i),
            "description": f"Auto-generated {i}",
            "change_reason": "sync",
        }
        results.append(result)

    with open("generated_contextputs.json", "w") as f:
        json.dump(results, f, indent=2)

    print("✔ generated_contextputs.json created")


if __name__ == "__main__":
    main()
