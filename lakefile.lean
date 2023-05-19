import Lake
open Lake DSL

package «parsec-extra» {
  -- add package configuration options here
}

@[default_target]
lean_lib «ParsecExtra» {
  -- add library configuration options here
}

-- dev is so not everyone has to build it
meta if get_config? env = some "dev" then
  require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
  require «lspec» from git "https://github.com/lurk-lab/LSpec" @ "main"
