defmodule ReltoolUtil.Mixfile do
  use Mix.Project

  def project do
    [app: :reltool_util,
     version: "1.3.3",
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
  end

  defp description do
    "Erlang reltool utility functionality application"
  end

  defp package do
    [files: ~w(src doc release scope rebar.config README.markdown),
     contributors: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/reltool_util"}]
   end
end
