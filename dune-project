(lang dune 3.13)
(name figma_mcp)
(version 0.3.1)

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
  (mcp_protocol (>= 0.1.0))
  (grpc-direct (>= 0.1.0))
  ; Standard dependencies
  (yojson (>= 2.0))
  (cohttp-lwt-unix (>= 5.0))
  (conduit-lwt-unix (>= 5.0))
  (lwt (>= 5.6))
  (uri (>= 4.2))
  (cmdliner (>= 1.1))
  (eio (>= 1.0))
  (ppx_deriving_yojson (>= 3.6))))
