// Blade tile cache: revision reuse across immutable dataset snapshots
// (docs/plans/structural/04-revision-reuse.md). A pure, index-local traversal
// over Icechunk reads is emitted one leading-axis TILE at a time; every tile
// has a compile-time key -- a SHA-256 over the task's text, the output
// geometry, and the content identities of exactly the input chunks the tile
// reads -- and a local on-disk store keyed that way lets a run against a
// later snapshot skip the recomputation (and the chunk reads) of every tile
// whose dependencies did not move. Values only, never text: what prints is
// the materialized array either way, so cold and warm stdout are identical.
//
// The store: BLADE_TILE_CACHE, read at RUN time with the exe cache's grammar
// -- unset / `0` / `off` -> disabled (every tile computes, nothing stores),
// `1` / `on` / `true` -> %LOCALAPPDATA%\Blade\tile-cache (or ~/.cache/blade/
// tile-cache), an absolute path -> that directory, anything else -> disabled.
// Files are `<dir>/<key[0:2]>/<key>-<toolchain>.tile`: the toolchain id
// (`-DBLADE_TOOLCHAIN_ID=...`, Build.fs) keeps bits compiled by a different
// compiler / flags / CPU selection apart without hashing at run time. A file
// is a 32-byte header { "BLTL", version, payload bytes } plus the tile's
// cells, row-major; a header that does not match what the program expects is
// a miss, never a value. Stores are temp-file-then-rename; two processes
// storing one key write identical bytes, so a race is benign.
//
// Deliberately light, like blade_run_record.hpp: C stdio only, cold code.
#pragma once
#if !defined(__CUDA_ARCH__)
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#if defined(_WIN32)
#include <direct.h>
#define BLADE_TILES_MKDIR(p) _mkdir(p)
#else
#define BLADE_TILES_MKDIR(p) mkdir(p, 0755)
#endif

#ifndef BLADE_TOOLCHAIN_ID
#define BLADE_TOOLCHAIN_ID unknown
#endif
#define BLADE_TILES_STR_(x) #x
#define BLADE_TILES_STR(x) BLADE_TILES_STR_(x)
#if defined(__GNUC__)
#define BLADE_TILES_COLD __attribute__((noinline, cold))
#else
#define BLADE_TILES_COLD
#endif

namespace blade_tiles {

  struct Header {
    char magic[4];          // "BLTL"
    std::uint32_t version;  // 1
    std::uint64_t bytes;    // payload size
    std::uint64_t reserved0;
    std::uint64_t reserved1;
  };

  // The store directory, resolved once; "" = disabled.
  BLADE_TILES_COLD inline const std::string& dir() {
    static std::string resolved;
    static bool done = false;
    if (done) return resolved;
    done = true;
    const char* v = std::getenv("BLADE_TILE_CACHE");
    std::string s = v ? v : "";
    auto lower = [](std::string t) { for (auto& c : t) c = (char)((c >= 'A' && c <= 'Z') ? c + 32 : c); return t; };
    std::string l = lower(s);
    if (s.empty() || l == "0" || l == "off" || l == "false") { resolved = ""; return resolved; }
    if (l == "1" || l == "on" || l == "true") {
#if defined(_WIN32)
      const char* base = std::getenv("LOCALAPPDATA");
      if (!base || !*base) { resolved = ""; return resolved; }
      resolved = std::string(base) + "\\Blade\\tile-cache";
#else
      const char* home = std::getenv("HOME");
      if (!home || !*home) { resolved = ""; return resolved; }
      resolved = std::string(home) + "/.cache/blade/tile-cache";
#endif
      return resolved;
    }
    bool rooted = (s.size() > 0 && (s[0] == '/' || s[0] == '\\')) || (s.size() > 2 && s[1] == ':');
    resolved = rooted ? s : "";
    return resolved;
  }

  inline bool enabled() { return !dir().empty(); }

  BLADE_TILES_COLD inline bool verbose() {
    const char* v = std::getenv("BLADE_TILE_CACHE_VERBOSE");
    return v && *v && !(v[0] == '0' && v[1] == '\0');
  }

  BLADE_TILES_COLD inline std::string path_of(const char* key) {
    std::string d = dir();
    if (d.empty()) return "";
    std::string sub = d + "/" + std::string(key, 2);
    BLADE_TILES_MKDIR(d.c_str());
    BLADE_TILES_MKDIR(sub.c_str());
    return sub + "/" + key + "-" + BLADE_TILES_STR(BLADE_TOOLCHAIN_ID) + ".tile";
  }

  // A stored tile of exactly `bytes` payload bytes exists under `key`.
  BLADE_TILES_COLD inline bool probe(const char* key, std::uint64_t bytes) {
    if (!enabled()) return false;
    std::string p = path_of(key);
    std::FILE* f = std::fopen(p.c_str(), "rb");
    if (!f) return false;
    Header h;
    bool ok = std::fread(&h, sizeof h, 1, f) == 1
              && std::memcmp(h.magic, "BLTL", 4) == 0 && h.version == 1 && h.bytes == bytes;
    std::fclose(f);
    return ok;
  }

  // Read the tile under `key` into `dst` (exactly `bytes` bytes). False on a
  // miss or a mismatched header; a short read after a valid header is a
  // corrupt store and dies loudly rather than handing back partial cells.
  BLADE_TILES_COLD inline bool load(const char* key, void* dst, std::uint64_t bytes) {
    if (!enabled()) return false;
    std::string p = path_of(key);
    std::FILE* f = std::fopen(p.c_str(), "rb");
    if (!f) return false;
    Header h;
    if (std::fread(&h, sizeof h, 1, f) != 1
        || std::memcmp(h.magic, "BLTL", 4) != 0 || h.version != 1 || h.bytes != bytes) {
      std::fclose(f);
      return false;
    }
    std::size_t got = std::fread(dst, 1, (std::size_t)bytes, f);
    std::fclose(f);
    if (got != bytes) {
      std::fprintf(stderr, "Blade tile cache error: '%s' is short (%llu of %llu payload bytes) -- a corrupt store; delete it\n",
                   p.c_str(), (unsigned long long)got, (unsigned long long)bytes);
      std::exit(1);
    }
    return true;
  }

  // Store `bytes` bytes from `src` under `key`: temp file, then rename. An
  // existing file under the key holds the same bytes by construction and is
  // left alone.
  BLADE_TILES_COLD inline void store(const char* key, const void* src, std::uint64_t bytes) {
    if (!enabled()) return;
    std::string p = path_of(key);
    if (probe(key, bytes)) return;
    std::string tmp = p + ".tmp" + std::to_string((unsigned long long)std::rand());
    std::FILE* f = std::fopen(tmp.c_str(), "wb");
    if (!f) return;
    Header h;
    std::memcpy(h.magic, "BLTL", 4);
    h.version = 1; h.bytes = bytes; h.reserved0 = 0; h.reserved1 = 0;
    bool ok = std::fwrite(&h, sizeof h, 1, f) == 1 && std::fwrite(src, 1, (std::size_t)bytes, f) == bytes;
    std::fclose(f);
    if (!ok) { std::remove(tmp.c_str()); return; }
    std::remove(p.c_str());   // a concurrent identical store may have won; harmless either way
    if (std::rename(tmp.c_str(), p.c_str()) != 0) std::remove(tmp.c_str());
  }
}
#endif
