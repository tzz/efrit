#!/usr/bin/env python3
"""Stand-in for curl that answers efrit-media's generation requests.

efrit-media runs `curl --config FILE --data-binary @BODY ... URL`.  The
scenario is the URL's last path element; the body and the config file
are echoed into the environment-named capture file (MOCK_MEDIA_CAPTURE)
so a test can check what was sent.

  .../images      -> OpenAI images shape: {"data":[{"b64_json": PNG}]}
  .../interactions -> Interactions shape with a video block (MP4 bytes)
  .../image-interactions -> Interactions shape with an image block
  .../error400    -> exit 22 (curl --fail-with-body) with a JSON error
  .../slow        -> sleep 30 s (the test cancels)
  .../garbage     -> exit 0 with text that is not JSON
"""
import base64, json, os, sys, time

def arg_after(flag):
    a = sys.argv
    return a[a.index(flag) + 1] if flag in a else None

url = sys.argv[-1]
scenario = url.rstrip("/").rsplit("/", 1)[-1]
body_path = arg_after("--data-binary")
config_path = arg_after("--config")
body = open(body_path[1:]).read() if body_path else ""
config = open(config_path).read() if config_path else ""

capture = os.environ.get("MOCK_MEDIA_CAPTURE")
if capture:
    with open(capture, "w") as f:
        json.dump({"url": url, "body": json.loads(body) if body else None,
                   "config": config, "argv": sys.argv[1:]}, f)

# A 1x1 PNG and a few bytes that start like an MP4 box.
PNG = base64.b64decode(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")
MP4 = b"\x00\x00\x00\x18ftypmp42" + b"\x00" * 24

def out(obj):
    sys.stdout.write(json.dumps(obj))
    sys.stdout.flush()

if scenario == "images":
    out({"created": 1, "data": [{"b64_json": base64.b64encode(PNG).decode()}]})
elif scenario == "interactions":
    out({"id": "i1", "steps": [
        {"type": "thinking", "content": []},
        {"type": "model_output", "content": [
            {"type": "text", "text": "here"},
            {"type": "video", "mime_type": "video/mp4", "data": base64.b64encode(MP4).decode()}]}]})
elif scenario == "image-interactions":
    out({"steps": [{"type": "model_output", "content": [
        {"type": "image", "data": base64.b64encode(PNG).decode()}]}]})
elif scenario == "error400":
    out({"error": {"message": "prompt rejected by the safety system", "type": "invalid_request_error"}})
    sys.exit(22)
elif scenario == "slow":
    time.sleep(30)
elif scenario == "garbage":
    sys.stdout.write("<html>not json</html>")
else:
    sys.stderr.write("unknown scenario %s\n" % scenario)
    sys.exit(7)
