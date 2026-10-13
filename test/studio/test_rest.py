import io
import json
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from client import FigmaRest, TransportError

class Response(io.BytesIO):

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

class RestOpener:

    def __init__(self, version='v7', image=True):
        self.requests = []
        self.timeouts = []
        self.version, self.image = (version, image)

    def open(self, request, *, timeout):
        self.requests.append(request)
        self.timeouts.append(timeout)
        if '/nodes?' in request.full_url:
            value = {'version': self.version, 'nodes': {'1:2': {'document': {'id': '1:2', 'type': 'FRAME'}}}}
        elif '/v1/images/' in request.full_url:
            value = {'images': {'1:2': 'https://cdn.example.test/asset.png' if self.image else None}}
        else:
            return Response(b'\x89PNG\r\n\x1a\nfixture')
        return Response(json.dumps(value).encode())

class TransportTests(unittest.TestCase):

    def test_rest_capture_pins_render_version_and_never_sends_pat_to_asset(self):
        rest = FigmaRest('private-pat', 100000, request_timeout_seconds=0.25)
        opener = RestOpener()
        rest.opener = opener
        capture = rest.capture('file-key', '1:2')
        self.assertEqual(capture['version'], 'v7')
        self.assertEqual(capture['source'], 'rest_version')
        self.assertIn('version=v7', opener.requests[1].full_url)
        self.assertEqual(opener.requests[0].get_header('X-figma-token'), 'private-pat')
        self.assertIsNone(opener.requests[2].get_header('X-figma-token'))
        self.assertIsNone(opener.requests[2].get_header('Authorization'))
        self.assertEqual(opener.timeouts, [0.25, 0.25, 0.25])
        rest.opener = RestOpener(version='wrong')
        with self.assertRaises(TransportError):
            rest.capture('file-key', '1:2', 'v7')
        rest.opener = RestOpener(image=False)
        with self.assertRaises(TransportError):
            rest.capture('file-key', '1:2')
