import re
import collections

def parse_result_line(line):
    match = re.match(r"(.+): (.+)", line)
    if match is None:
        raise RuntimeError("Invalid result line: " + line)
    return { match.group(1): match.group(2) }

def parse_results(output):
    results = [parse_result_line(l) for l in output.splitlines()]
    return dict(collections.ChainMap(*results))

def show_results(results):
    return "\n".join(f"{key}: {value}" for key, value in results.items())