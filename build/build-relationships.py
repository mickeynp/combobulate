#!/usr/bin/env python
# -*- coding: utf-8; mode: python -*-

import requests
from pathlib import Path
import itertools
import argparse


from collections import defaultdict
import json
import logging
from dataclasses import dataclass
from pprint import pprint as pp
from typing import List

logging.basicConfig()
log = logging.getLogger(__name__)
log.setLevel(logging.INFO)

SOURCES = {
    "yaml": {
        "nodes": "https://raw.githubusercontent.com/ikatyang/tree-sitter-yaml/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/ikatyang/tree-sitter-yaml/master/src/grammar.json",
    },
    "tsx": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-typescript/master/tsx/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-typescript/master/tsx/src/grammar.json",
    },
    "css": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-css/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-css/master/src/grammar.json",
    },
    "typescript": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-typescript/master/typescript/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-typescript/master/typescript/src/grammar.json",
    },
    "javascript": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-javascript/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-javascript/master/src/grammar.json",
    },
    "jsx": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-javascript/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-javascript/master/src/grammar.json",
    },
    "go": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-go/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-go/master/src/grammar.json",
    },
    "python": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-python/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-python/master/src/grammar.json",
    },
    "c": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-c/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-c/master/src/grammar.json",
    },
    "html": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-html/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-html/master/src/grammar.json",
    },
    "toml": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-toml/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-toml/master/src/grammar.json",
    },
    "json": {
        "nodes": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-json/master/src/node-types.json",
        "grammar": "https://raw.githubusercontent.com/tree-sitter/tree-sitter-json/master/src/grammar.json",
    },
}

DEFAULT_FIELD_NAME = "*unnamed*"


def lisp_string(s):
    return '"%s"' % s


class Symbol(str):
    pass


class Quote(list):
    pass


class LispString(str):
    pass


def sexp(args):
    match args:
        case dict() as a:
            plist = []
            for k, v in a.items():
                plist.extend([Symbol(":" + k), v])
            return sexp(plist)
        case Quote() as a:
            return "'" + "".join(a)
        case LispString() as a if a:
            return lisp_string(a)
        case (Symbol() | str()) as a:
            return a
        case (set() | list()) as arg if arg:
            return Symbol("(") + " ".join(sexp(a) for a in arg if a) + Symbol(")")
        case None:
            return Symbol("nil")


def match_rule(obj, rules):
    def handle_types(types):
        children = set()
        for child_type in types:
            match child_type:
                case {"named": True, "type": child_type}:
                    if child_type.startswith("_"):
                        children |= rules[child_type][DEFAULT_FIELD_NAME]
                    else:
                        children.add(LispString(child_type))
        return children

    match obj:
        case {"type": t, "named": True, "subtypes": st}:
            t = LispString(t)
            rules[t][DEFAULT_FIELD_NAME] |= handle_types(st)
        # Canonical case
        case {
            "fields": fields,
            "named": True,
            "type": t,
            **rest,
        }:
            t = LispString(t)
            try:
                types = rest["children"]["types"]
                rules[t][DEFAULT_FIELD_NAME] |= handle_types(types)
            except KeyError:
                pass
            for field_name, details in fields.items():
                rules[t][field_name] |= handle_types(details["types"])


def build_inverse_rules(rules):
    d = defaultdict(set)
    for rule, per_field_rules in rules.items():
        for field, sub_rules in per_field_rules.items():
            for sub_rule in sub_rules:
                d[sub_rule].add(rule)
    return d


def process_rules(data):
    results = []
    subtypes = {}
    rules = defaultdict(lambda: defaultdict(set))
    for obj in data:
        result = match_rule(obj, rules)
    inv_rules = build_inverse_rules(rules)
    return rules, inv_rules


def generate_sexp(rules, inv_rules, language, source):
    l = []
    for rule_name, rule_fields in rules.items():
        # Ignore named fields and only generate the defaults?
        # without named fields.
        for field_name, field_values in rule_fields.items():
            if not field_values:
                rule_fields[field_name] = Symbol("nil")
        rules = itertools.chain.from_iterable(rule_fields.values())
        l.append(sexp([rule_name, rule_fields]))
        l.append("\n")
    return [
        sexp(
            [
                Symbol("defconst"),
                Symbol(f"combobulate-rules-{language}"),
                "\n",
                Quote(sexp(sexp(l))),
            ]
        ),
        sexp(
            [
                Symbol("defconst"),
                Symbol(f"combobulate-rules-{language}-inverted"),
                "\n",
                Quote(sexp([sexp([k, v]) + "\n  " for k, v in inv_rules.items()])),
                "\n",
            ]
        ),
    ]


def load_node_types(source):
    assert isinstance(source, Path), f"{source} must be a Path-like object."
    try:
        return json.loads(source.read_text())
    except Exception:
        log.critical(
            "Cannot find source %s. Try downloading the sources first with `--download'.",
            source,
        )
        raise


def download_source(source, output_filename, url):
    log.info("Downloading source %s. Output filename: %s", source, output_filename)
    r = requests.get(url)
    r.raise_for_status()
    d = r.text
    Path(output_filename).write_text(d)
    return d


def download_all_sources():
    for source, files in SOURCES.items():
        download_source(source, f"{source}-nodes.json", files["nodes"])
        download_source(source, f"{source}-grammar.json", files["grammar"])


def write_elisp_file(forms):
    log.info("Writing forms to file")
    with Path("../combobulate-rules.el").open("w") as f:
        for src, (form, inv_form) in forms:
            if not form:
                log.error("Skipping %s as it is empty", src)
                continue
            f.write(f";; START Auto-generated production rules for `{src}'\n")
            f.write(form)
            f.write("\n")
            f.write("\n")
            f.write(inv_form)
            f.write("\n")
            f.write(f";; END production rules for {src}\n")
            f.write("\n" * 5)
        f.write("(provide 'combobulate-rules)\n")


def parse_source(language, source):
    log.info("Parsing language %s with node file %s", language, source)
    data = load_node_types(source)
    rules, inv_rules = process_rules(data)
    return generate_sexp(rules, inv_rules, language, source)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--download", action="store_true", help="Download sources first", default=False
    )
    args = parser.parse_args()
    forms = []
    if args.download:
        download_all_sources()

    for src, files in SOURCES.items():
        forms.append((src, parse_source(src, Path(f"{src}-nodes.json"))))
    write_elisp_file(forms)


if __name__ == "__main__":
    main()
