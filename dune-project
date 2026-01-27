(lang dune 3.15)
(name figma_mcp)
(version 0.3.9)

(generate_opam_files true)

(source
 (github jeong-sik/figma-mcp))

(authors "Jeongsik Yun")
(maintainers "Jeongsik Yun")
(license MIT)

(package
 (name figma_mcp)
 (synopsis "Figma MCP Server - OCaml 5.x Native with gRPC Streaming")
 (description "Type-safe Figma JSON parser with MCP protocol support and gRPC streaming for large files")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.13))
  ; External dependencies (opam pin)
  (grpc-direct (>= 0.1.0))
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
  (ipaddr (>= 5.0))))
