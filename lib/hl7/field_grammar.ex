defmodule Parser do
  @moduledoc """
  An HL7 Path is defined as:

  SEG[SEG_NUM]-F[REPEAT_NUM][[.C].S]

  where:

  SEG – a 3 character segment name. For example PID.

  [SEG_NUM] – SEG_NUM is an integer value where SEG_NUM>0 or SEG_NUM='*'. If [SEG_NUM] is omitted, SEG[1] is assumed. If [SEG_NUM]='*', all matching segments are included in the search. For example, OBR[2] means the second OBR segment, whereas OBR[1] and OBR are equivalent.

  F – is an integer value where F>0. For example, PID-3 means the 3rd field in the PID segment.

  [REPEAT_NUM]– is the Field Repeat number where REPEAT_NUM>0 or REPEAT_NUM='*'. If REPEAT_NUM='*' all repeating fields are included in the query. If [REPEAT_NUM] is omitted, F[1] is assumed.

  C – C>0 or is absent. If C is absent, all components in the field are included separated by '^'. For example, PID-3 means PID-3.1 +PID-3.2+PID-3.3...PID-3.N.

  S – S>0 or is absent. If S is absent, all subcomponents in the component are included separated by '&'. If S is omitted, S=1 is assumed. For example, PID-3.2.3 means the third subcomponent of the component of PID-3, and PID-3.2 means PID-3.2.1+PID-3.2.2+PID-3.2.3...PID-3.2.N.
  """
  import NimbleParsec

  index = integer(min: 1)

  bracketed_num  =
    ignore(string("["))
    |> concat(choice([index, string("*")]))
    |> ignore(string("]"))
  
  defaulted_num =
    choice([bracketed_num, empty() |> replace(1)])
  
  dot = ignore(string("."))
  dash = ignore(string("-"))

  field = index |> tag(:field)

  segment_id =
      ascii_string([?A..?Z], 3) |> tag(:segment)
  
  segment_num =
    defaulted_num |> tag(:segment_number)

  repeat_num =
    defaulted_num |> tag(:repeat_num)
  
  component =
    index |> tag(:component)

  subcomponent =
    index |> tag(:subcomponent)
  
  component_part =
    dot
    |> concat(component)
    |> optional(concat(dot, subcomponent))

  indexes =
    repeat(
      index |> concat(optional(ignore(dot))))

  defparsec :path,
      segment_id
      |> concat(segment_num)
      |> concat(dash)
      |> concat(field)
      |> concat(repeat_num)
      |> optional(component_part)
      |> eos(),
      export_metadata: true
      
end

defmodule HL7.FieldGrammar do
  require Logger

  @derive Inspect

  defstruct [:data]

  @typep segment :: String.t()
  @typep indices :: [integer, ...]
  @type t :: %__MODULE__{
          data: {segment, indices} | indices
        }

  def to_string(%__MODULE__{data: {segment, indices}}) do
    "#{segment}-" <> Enum.join(indices, ".")
  end

  def to_string(%__MODULE__{data: indices}) when is_list(indices) do
    Enum.join(indices, ".")
  end

  defimpl String.Chars do
    def to_string(grammar) do
      HL7.FieldGrammar.to_string(grammar)
    end
  end

  @deprecated "Use FieldGrammar.new/1 instead"
  def to_indices(schema) do
    new(schema)
  end

  @spec new(String.t()) :: t()
  def new(schema) when is_binary(schema) do
    use_repeat = String.contains?(schema, "[")
    use_segment = String.contains?(schema, "-")
    use_component = String.contains?(schema, ".")
    chunks = chunk_schema(schema)
    [head | tail] = chunks

    data =
      case use_segment do
        true ->
          [<<segment::binary-size(3)>> | indices] =
            case use_component && !use_repeat do
              false ->
                [head | tail |> Enum.map(&String.to_integer/1)]

              true ->
                [head | tail |> Enum.map(&String.to_integer/1) |> List.insert_at(1, 1)]
            end
            |> Enum.take(5)
            |> Enum.with_index()
            |> Enum.map(fn {v, i} -> if i > 1, do: v - 1, else: v end)

          {segment, indices}

        false ->
          case use_component && !use_repeat do
            false ->
              chunks |> Enum.map(&String.to_integer/1)

            true ->
              chunks |> Enum.map(&String.to_integer/1) |> List.insert_at(1, 1)
          end
          |> Enum.take(4)
          |> Enum.with_index()
          |> Enum.map(fn {v, i} -> if i > 0, do: v - 1, else: v end)
      end

    %__MODULE__{data: data}
  end

  @spec chunk_schema(String.t()) :: [String.t()]
  defp chunk_schema(schema) do
    Regex.split(~r{(\.|\-|\[|\]|\s)}, schema, include_captures: false, trim: true)
  end
end
