defmodule Sfmt.Mixfile do
  use Mix.Project

  def project do
    [app: :sfmt,
     version: "0.12.0",
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
        "CHANGES*",
        "CONTRIBUTING*",
        "Doxyfile",
        "LICENSE",
        "Makefile*",
        "README.md",
        "erlang.mk",
        "package.exs"
        ],
     contributors: [
        "Kenji Rikitake",
        "Mutsuo Saito",
        "Makoto Matsumoto",
        "Dan Gudmundsson",
        "Michael Truog",
        "Michael Chmielewski"
        ],
     licenses: ["simplified BSD"],
     links: %{"GitHub" => "https://github.com/jj1bdx/sfmt-erlang/"}
     ]
  end
end
