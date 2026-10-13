# Figma Studio companion

The Studio Plugin edits the open Figma document. The host relay sends requests
between that Plugin and MASC's network-isolated `figma-studio` Lane Addon. The
same relay can capture server versions through Figma's REST API with a PAT.
No model or MASC credential runs in the Plugin.

## Prepare the local Plugin

1. In Figma Desktop, create/open a Design file.
2. In Plugins → Development, create a new development Plugin once and save its
   generated manifest. Figma assigns the Plugin identity; this checkout does not
   invent one. Prepare a separate import directory using that manifest:

   ```sh
   python3 studio/prepare.py --from-manifest /path/to/generated/manifest.json \
     --output /path/to/new/figma-studio-plugin
   ```

3. Import the resulting `figma-studio-plugin/manifest.json` in Development.
   Keep its adjacent `code.js` and `ui.html` files together.
4. Run **Figma Studio for MASC**. Select a frame or create a blank frame for
   the initial design. Leave the Plugin open while MASC works.

The development manifest allows `http://127.0.0.1:8941` for the local relay.
It uses stable APIs and has no proposed/private Plugin API dependency. Import
and live rendering still require verification in an actual Figma session.
See the official [manifest identity contract](https://developers.figma.com/docs/plugins/manifest/).

## Prepare the relay

Use Python 3.11 or newer; the companion uses the standard library.

```sh
python3 studio/relay.py init --config /path/to/private/figma-studio.toml
```

The new file is created with mode 0600. Set `masc.url` to the Full-profile MASC
MCP endpoint and `masc.token` to the credential for the Addon's configured
`relay_agent`. Set `figma.token` to the PAT when enabling REST capture. Keep
these values in the private file; never paste them into model prompts or the
Plugin. Only the separate generated pairing token belongs in the Plugin UI.

Build and install the MASC package from `addons/figma-studio` in the MASC
repository. Its installation binding identifies the Keeper (`leader` in the
example), relay agent and logical installation. A new binding uses a new
persistent installation; do not repurpose another Keeper's stored data.

```sh
python3 studio/relay.py serve --config /path/to/private/figma-studio.toml
# Run locally to obtain the value for the Plugin's pairing-token input:
python3 studio/relay.py pairing-token --config /path/to/private/figma-studio.toml
```

The relay binds to loopback, requires its pairing token, checks browser origins,
and never follows authenticated redirects. Request loss does not cause an edit
replay. A worker restart requires pairing again; original receipts remain in the
Addon. Stop the relay with Ctrl-C. Starting it does not launch or log into Figma.

## Capture, edit and verify

Ask the configured Keeper to list Studio sessions and bind the chosen one.
`masc_figma_submit` with `kind: capture` stores a native `JSON_REST_V1` export
and PNG. For edits, provide the original snapshot ID and a typed operation list.
The Plugin preflights targets, property values and fonts, then checks the
baseline before mutation. Changed node IDs and readback accompany the result.

`masc_figma_validate` evaluates explicit node/property assertions on a snapshot.
This proves those assertions only. The returned PNG is also available to the
Keeper for visual inspection. Code implementation must be checked in its real
application; native structure and image similarity do not establish interaction
behavior.

For REST capture, `masc_figma_rest_capture` accepts `operation_id`, `file_key`,
`root_id` and optional `version`; `masc_figma_rest_operation` reads its outcome.
The relay pins the render to the node response's version and retrieves PNG bytes
without sending the PAT to the asset host. This works without an open Plugin.
A REST snapshot is not automatically associated with an open editable document.

## Kitty preview

Save the complete `masc_figma_snapshot` tool result, including its image block:

```sh
python3 studio/preview.py snapshot-result.json --kitty
python3 studio/preview.py snapshot-result.json --output preview.png
# For redirected output or an environment without terminal-size detection:
python3 studio/preview.py snapshot-result.json --kitty --size 100 36
```

To download it directly, configure the optional private `[owner]` section with
an operator endpoint and credential, then run:

```sh
python3 studio/relay.py snapshot --config /path/to/private/figma-studio.toml \
  --snapshot-id SNAPSHOT_ID --output snapshot-result.json
python3 studio/preview.py snapshot-result.json --kitty
```

The relay agent is restricted to the relay port; it cannot read arbitrary owner
tools. The download command uses the separate owner credential only on the host.

The helper verifies the retained PNG hash before invoking `timg -pk`. It does
not overwrite an existing output file. Install `timg` separately if needed.
The same bytes are used for image review, download and terminal preview.
MASC's automatic tool-image-to-chat-preview path is a separate integration;
this helper does not claim that a PNG returned by a tool automatically appears
inside the TUI.

Standalone server capture is also available for diagnosis:

```sh
python3 studio/relay.py rest-capture --config /path/to/private/figma-studio.toml \
  --file-key FILE --node-id 1:2 --output server-snapshot.json
python3 studio/preview.py server-snapshot.json --kitty
```

## Verification and boundaries

```sh
npm run test:studio
npm run test:plugin
python3 -m unittest discover -s test/studio -p 'test_*.py' -v
ruff check studio test/studio/test_relay.py
pyright studio
# Cross-repository contract run; creates a new evidence directory:
python3 test/studio/contract_smoke.py --worker /path/to/masc/addons/figma-studio \
  --output /path/to/new/evidence-directory
```

Fixtures execute the actual Plugin entrypoint and UI script against controlled
native/document and relay substitutes. HTTP/SSE tests exercise real local
transport; the Kitty test invokes installed timg. These are separate from live
Figma, PAT access, installed MASC/Docker and human-visible display evidence.
The contract run uses the real worker process, relay HTTP transport and Plugin
entrypoint with a controlled document and a host-routing fixture; it retains
the exact snapshot result, PNG, structural report and Kitty protocol bytes.
