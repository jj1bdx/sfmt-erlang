defmodule Sfmt.Mixfile do
  use Mix.Project

  def project do
    [app: :sfmt,
     version: "0.12.4",
     description: description,
     package: package,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end

  defp description do
    """
    SIMD-oriented Fast Mersenne Twister (SFMT) for Erlang.
    """
  end

  defp package do
    [files: [
        "c_src",
        "priv",
        "reference_texts",
        "src",
        "test",
        "test_scripts",
        ".gitignore",
        ".travis.yml",
        "CHANGES.md",
        "CONTRIBUTING.md",
        "Doxyfile",
        "LICENSE",
        "Makefile",
        "Makefile.sfmt",
        "README.md",
        "erlang.mk",
        "package.exs"
        ],
     maintainers: [
        "Kenji Rikitake"
        ],
     licenses: ["simplified BSD"],
     links: %{"GitHub" => "https://github.com/jj1bdx/sfmt-erlang/"}
     ]
  end
end
