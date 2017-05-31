#-*-Mode:elixir;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
# ex: set ft=elixir fenc=utf-8 sts=2 ts=2 sw=2 et nomod:

defmodule ReltoolUtil.Mixfile do
  use Mix.Project

  def project do
    [app: :reltool_util,
     version: "1.7.0",
     language: :erlang,
     description: description(),
     package: package(),
     deps: deps()]
  end

  defp deps do
    []
  end

  defp description do
    "Erlang reltool utility functionality application"
  end

  defp package do
    [files: ~w(src doc release scope ex2erl rebar.config README.markdown),
     maintainers: ["Michael Truog"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/okeuday/reltool_util"}]
   end
end
