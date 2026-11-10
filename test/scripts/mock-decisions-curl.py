#!/usr/bin/env python3
"""Stand-in for curl that answers efrit-decisions requests.

Reads the JSON body (`--data-binary @FILE`), answers every question in
order with values derived from the input text, so a test can predict
them:

  predicate -> probability 0.9 if the input contains "yes", else 0.1
  choice    -> the first choice whose value appears in the input, else the last;
               probabilities put 0.8 on it and spread the rest
  score     -> the index of the first level label found in the input, else 1.5

URL scenarios (last path element):
  .../decisions  -> normal
  .../error400   -> exit 22 with a JSON error
  .../slow       -> sleep 0.3 s then normal (for ordering/parallel tests)
  .../short      -> one answer fewer than asked

The body and the config file are recorded in MOCK_DECISIONS_CAPTURE
(appended, one JSON per line) when that variable is set.
"""
import json, os, sys, time

def arg_after(flag):
    a = sys.argv
    return a[a.index(flag) + 1] if flag in a else None

url = sys.argv[-1]
scenario = url.rstrip("/").rsplit("/", 1)[-1]
body = json.load(open(arg_after("--data-binary")[1:]))
config = open(arg_after("--config")).read()

capture = os.environ.get("MOCK_DECISIONS_CAPTURE")
if capture:
    with open(capture, "a") as f:
        f.write(json.dumps({"url": url, "body": body, "config": config}) + "\n")

text = (body.get("input") or "").lower()

def answer(q):
    t = q["type"]
    if t == "predicate":
        return {"probability": 0.9 if "yes" in text else 0.1}
    if t == "choice":
        values = [c["value"] for c in q["choices"]]
        pick = next((v for v in values if v.lower() in text), values[-1])
        rest = (0.2 / (len(values) - 1)) if len(values) > 1 else 0
        return {"choice": pick, "probabilities": {v: (0.8 if v == pick else rest) for v in values}}
    if t == "score":
        labels = [l["label"] for l in q["levels"]]
        idx = next((i for i, l in enumerate(labels) if l.lower() in text), None)
        return {"score": float(idx) if idx is not None else 1.5}
    raise SystemExit("unknown question type %s" % t)

if scenario == "error400":
    sys.stdout.write(json.dumps({"error": {"message": "model not available", "type": "invalid_request_error"}}))
    sys.exit(22)
if scenario == "slow":
    time.sleep(0.3)
answers = [answer(q) for q in body["questions"]]
if scenario == "short":
    answers = answers[:-1]
sys.stdout.write(json.dumps({"id": "d1", "model": body.get("model"), "answers": answers}))
