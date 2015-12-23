defmodule ReltoolUtil.Mixfile do
  use Mix.Project

  def project do
    [app: :reltool_util,
     version: "1.5.1",
     language: :erlang,
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
    []
  end

  defp description do
    "Erlang reltool utility functionality application"
  end

  defp package do
    [files: ~w(src doc release scope rebar.config README.markdown),
     maintainers: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/reltool_util"}]
   end
end
