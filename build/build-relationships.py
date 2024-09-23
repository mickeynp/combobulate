#!/usr/bin/env python

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
import configparser

logging.basicConfig()
log = logging.getLogger(__name__)
log.setLevel(logging.INFO)


DEFAULT_FIELD_NAME = "*unnamed*"


def lisp_string(s):
    return '"%s"' % s


class Symbol(str):
    pass


class Quote(list):
    pass


class BackQuote(list):
    pass


class LispString(str):
    pass


class Splice(list):
    pass


def sexp(args):
    match args:
        case dict() as a:
            plist = []
            for k, v in a.items():
                plist.extend([Symbol(":" + k), v])
            return sexp(plist)
        case Splice() as a:
            return Symbol("," + "".join(a))
        case BackQuote() as a:
            return "`" + "".join(a)
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
                        children |= rules[LispString(child_type)][DEFAULT_FIELD_NAME]
                    else:
                        children.add(LispString(child_type))
        return children

    match obj:
        case {"type": t, "named": True, "subtypes": st}:
            t = LispString(t)
            rules[t][DEFAULT_FIELD_NAME] |= handle_types(st)
        case {
            "fields": fields,
            "named": True,
            "type": t,
            **rest,
        }:
            t = LispString(t)
            # do this for the side-effect of creating a `t' entry.
            rules[t][DEFAULT_FIELD_NAME]
            try:
                types = rest["children"]["types"]
                rules[t][DEFAULT_FIELD_NAME] |= handle_types(types)
            except KeyError:
                pass
            for field_name, details in fields.items():
                rules[t][field_name] |= handle_types(details["types"])
        case {"named": True, "type": str() as child_type, **rest}:
            t = LispString(child_type)
            # We are relying on the side-effect of the defaultdict
            # factory here.
            rules[t][DEFAULT_FIELD_NAME]


def build_inverse_rules(rules):
    d = defaultdict(set)
    for rule, per_field_rules in rules.items():
        for field, sub_rules in per_field_rules.items():
            for sub_rule in sub_rules:
                d[sub_rule].add(rule)
    return d


def build_all_node_types(rules):
    types = set()
    for rule, per_field_rules in rules.items():
        types.add(rule)
        for field, sub_rules in per_field_rules.items():
            for sub_rule in sub_rules:
                types.add(sub_rule)
    return types


def build_supertypes(data):
    return [LispString(el) for el in data.get("supertypes", [])]


def process_grammar(data):
    return build_supertypes(data)


def process_nodes(data):
    rules = defaultdict(lambda: defaultdict(set))
    for obj in data:
        match_rule(obj, rules)
    inv_rules = build_inverse_rules(rules)
    all_node_types = build_all_node_types(rules)
    return rules, inv_rules, all_node_types


def make_rules_symbol(language, *rest):
    return Symbol(f"combobulate-rules-{language}" + "".join(rest))


def generate_sexp(rules, inv_rules, all_node_types, supertypes, language):
    l = []
    for rule_name, rule_fields in sorted(rules.items()):
        # Ignore named fields and only generate the defaults?
        # without named fields.
        for field_name, field_values in sorted(rule_fields.items()):
            if not field_values:
                rule_fields[field_name] = Symbol("nil")
        rules = itertools.chain.from_iterable(rule_fields.values())
        l.append(sexp([rule_name, rule_fields]))
        l.append("\n")
    return [
        sexp(
            [
                Symbol("defconst"),
                make_rules_symbol(language),
                "\n",
                Quote(sexp(sexp(l))),
            ]
        ),
        sexp(
            [
                Symbol("defconst"),
                make_rules_symbol(language, "-inverse"),
                "\n",
                Quote(
                    sexp([sexp([k, v]) + "\n  " for k, v in sorted(inv_rules.items())])
                ),
                "\n",
            ]
        ),
        sexp(
            [
                Symbol("defconst"),
                make_rules_symbol(language, "-types"),
                "\n",
                Quote(sexp([n for n in sorted(all_node_types)])),
                "\n",
            ]
        ),
        sexp(
            [
                Symbol("defconst"),
                make_rules_symbol(language, "-supertypes"),
                "\n",
                Quote(sexp([n for n in sorted(supertypes)]) or []) or "nil",
                "\n",
            ]
        ),
    ]


def load_json(source):
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


def download_all_sources(sources):
    for source, files in sources.items():
        download_source(source, f"{source}-nodes.json", files["nodes"])
        download_source(source, f"{source}-grammar.json", files["grammar"])


def write_elisp_file(forms):
    log.info("Writing forms to file")
    with Path("../combobulate-rules.el").open("w") as f:
        def newline():
            f.write("\n")

        def write_line(line):
            f.write(line)
            newline()

        def write_form(form, header: str | None = None, footer: str | None = None):
            if header:
                f.write(f";; START {header}")
                newline()
            f.write(form)
            newline()
            if footer:
                f.write(f";; END {footer}")
                newline()

        write_line(";; -*- lexical-binding: t -*-")
        write_line(";; This file is generated by build-relationships.py")
        write_line(";; Do not edit this file directly.")
        newline()
        langs = []

        for src, (form, inv_form, all_node_types_form, supertypes_form) in forms:
            langs.append(src)
            if not form:
                log.error("Skipping %s as it is empty", src)
                continue
            write_form(
                form,
                header=f"Production rules for {src}",
                footer=f"Production rules for {src}",
            )
            write_form(
                inv_form,
                header=f"Inverse production rules for {src}",
                footer=f"Inverse production rules for {src}",
            )
            write_form(
                all_node_types_form,
                header=f"All node types in {src}",
                footer=f"All node types in {src}",
            )
            write_form(
                supertypes_form,
                header=f"All supertypes in {src}",
                footer=f"All supertypes in {src}",
            )
            newline()
        write_form(
            sexp(
                [
                    Symbol("defconst"),
                    Symbol(f"combobulate-rules-languages"),
                    "\n",
                    Quote(sexp(sexp(sorted(langs)))),
                    "\n",
                    LispString(
                        "A list of all the languages that have production rules."
                    ),
                ]
            ),
            header="Auto-generated list of all languages",
            footer="Auto-generated list of all languages",
        )

        def make_alist(rules_symbol_extra):
            return BackQuote(
                sexp(
                    [
                        sexp(
                            [
                                Symbol(lang),
                                Splice(make_rules_symbol(lang, rules_symbol_extra)),
                            ]
                        )
                        + "\n"
                        for lang in sorted(langs)
                    ]
                )
            )

        for rules_symbol_extra in ["", "-inverse", "-types", "-supertypes"]:
            write_form(
                sexp(
                    [
                        Symbol("defconst"),
                        Symbol(f"combobulate-rules{rules_symbol_extra}-alist"),
                        "\n",
                        make_alist(rules_symbol_extra),
                    ]
                ),
            )

            newline()
        write_form(
            sexp(
                [
                    Symbol("provide"),
                    Quote(Symbol("combobulate-rules")),
                ]
            )
        )


def parse_source(language, nodes_fn, grammar_fn):
    log.info(
        "Parsing language %s with node file %s and grammar file %s",
        language,
        nodes_fn,
        grammar_fn,
    )
    node_data = load_json(nodes_fn)
    grammar_data = load_json(grammar_fn)
    supertypes = process_grammar(grammar_data)
    rules, inv_rules, all_node_types = process_nodes(node_data)
    return generate_sexp(rules, inv_rules, all_node_types, supertypes, language)


def read_sources(sources_file: str) -> dict:
    # Create a ConfigParser object
    sources = {}
    config = configparser.ConfigParser()
    config.read(sources_file)
    for section in config.sections():
        sources[section] = dict(config[section])
    return sources


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--download", action="store_true", help="Download sources first", default=False
    )
    parser.add_argument(
        "--sources-file", action="store", help="Sources file", default="sources.ini"
    )
    args = parser.parse_args()
    forms = []
    sources = read_sources(args.sources_file)
    if args.download:
        download_all_sources(sources)

    for src, files in sources.items():
        forms.append(
            (
                src,
                parse_source(
                    src, Path(f"{src}-nodes.json"), Path(f"{src}-grammar.json")
                ),
            )
        )
    write_elisp_file(forms)


if __name__ == "__main__":
    main()
