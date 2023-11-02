# Configer

Configer is a build tool to generate `.sys.config` files using layered YAML files.

## Example Usage

**Yaml Config**
```yaml
# A spec can only be included by other specs if this flag is marked 'true'
can_be_included: true
# Includes is a list of relative file paths that need to be evaluated before the defines & hosts sections of this spec can be evaluated
includes: []
# Defines is a collection of pre-defined properties which can be built on.
defines:
  $definition_name:
    builds_on: []
    properties: []
# Configs is a collection of output files comprised of the Definitions used as a base and explicitly set properties.
# Configs cannot be used as a base for other configs.
configs:
  $name:
    builds_on: []
    properties:
      kernel:
        - logger_level: notice
        - logger: "[{handler, default, logger_std_h, #{config => #{type => {file, \"path/to/file.log\"}}}}]"
```

**Output sys.config**

`$name.sys.config`
```erlang
[
	{kernel, [
		{logger_level,notice}
		{logger,[{handler, default, logger_std_h, #{config => #{type => {file, "path/to/file.log"}}}}]}
	]}
].
```