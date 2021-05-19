from ksc.rewrites import parse_rule_str

index_of_build = parse_rule_str(
    """(rule "index_of_build"
    ((idx : Integer) (n : Integer) (e : Any))
    (index idx (build n (lam (i : Integer) e)))
    (let (i idx) e))""",
    {},
)
