import csv, glob, os, sys

sp = os.path.dirname(os.path.abspath(__file__))
rows = {}
for d in ["pass1", "pass2"]:
    for f in glob.glob(os.path.join(sp, d, "part-*.csv")):
        for r in csv.DictReader(open(f)):
            rows.setdefault(int(r["scenario"]), {})
        # pass2 overrides pass1 cell by cell
        sc = int(os.path.basename(f)[5:-4])
        rows[sc] = list(csv.DictReader(open(f)))

def g(rs, test, bw=None):
    for r in rs:
        if r["test"] == test and (bw is None or (r["bandwidth"] not in ("", "NA") and abs(float(r["bandwidth"]) - bw) < 1e-9)):
            return float(r["rejection_rate"]) if r["rejection_rate"] not in ("", "NA") else float("nan")
    return float("nan")

hyp = sys.argv[1] if len(sys.argv) > 1 else "null"
print("sc form  truth       err  n    reps  wz.15  [wz.10 wz.25]  asym  form_x form_d fail bfail")
for sc in sorted(rows):
    rs = rows[sc]
    if not isinstance(rs, list) or not rs:
        continue
    r0 = rs[0]
    if hyp != "all" and r0["hypothesis"] != hyp:
        continue
    fails = sum(int(r["failures"]) for r in rs if r["test"] == "wz_bootstrap")
    bf = [r["bootstrap_failures"] for r in rs if r["test"] == "wz_bootstrap"][0]
    print("%2d %-5s %-11s %-4s %3s %5s  %.4f [%.3f %.3f]  %.3f  %.4f %.4f %s %s" % (
        sc, r0["form"][:5], r0["truth"], r0["errors"][:4], r0["n"], r0["replications"],
        g(rs, "wz_bootstrap", 0.15), g(rs, "wz_bootstrap", 0.10), g(rs, "wz_bootstrap", 0.25),
        g(rs, "wz_asymptotic", 0.15), g(rs, "form_test_x"), g(rs, "form_test_default"), fails, bf))
