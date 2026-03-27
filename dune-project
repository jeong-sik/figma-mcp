(lang dune 3.15)
(name figma_mcp)
(version 0.11.0)

(generate_opam_files true)

(source
 (github jeong-sik/figma-mcp))

(authors "Jeongsik Yun")
(maintainers "Jeongsik Yun")
(license MIT)

(package
 (name figma_mcp)
 (synopsis "Figma MCP Server - fidelity-focused design context and verification")
 (description "OCaml MCP server for Figma design context extraction, Code Connect lookup, and semantic or visual verification")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.13))
  (mcp_protocol (>= 1.0.0))
  ; Standard dependencies
  (yojson (>= 2.0))
  (uri (>= 4.2))
  (cmdliner (>= 1.1))
  (ppx_deriving_yojson (>= 3.6))
  ; Eio ecosystem
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (httpun-eio (>= 0.1))
  (gluten-eio (>= 0.4))
  (cohttp-eio (>= 6.0))
  (tls-eio (>= 1.0))
  (ca-certs (>= 0.2))
  (bigstringaf (>= 0.9))
  (mirage-crypto-rng (>= 1.0))
  (ipaddr (>= 5.0))
  "dune-build-info"
  (alcotest (and :with-test (>= 1.8.0)))
  (bisect_ppx (and :with-test (>= 2.8)))))
