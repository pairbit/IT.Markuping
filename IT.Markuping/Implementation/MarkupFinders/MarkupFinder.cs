using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

public class MarkupFinder<T> : BaseMarkupFinder<T> where T : unmanaged, IEquatable<T>
{
    public readonly struct Tokens
    {
        internal readonly T _lt;
        internal readonly T _gt;
        internal readonly T _slash;
        internal readonly T _colon;
        internal readonly T _space;//" "
        internal readonly T _quot;//"
        internal readonly T _eq;//=
        internal readonly T _apos;//'

        public Tokens(T lt, T gt, T slash, T colon, T space, T quot, T eq, T apos)
        {
            _lt = lt;
            _gt = gt;
            _slash = slash;
            _colon = colon;
            _space = space;
            _quot = quot;
            _eq = eq;
            _apos = apos;
        }

        public Tokens(MarkupTokens<T> tokens) :
            this(tokens._lt, tokens._gt, tokens._slash, tokens._colon, tokens._space, tokens._quot, tokens._eq, tokens._apos)
        {
            //if (Unsafe.SizeOf<MarkupTokens<T>>() < Unsafe.SizeOf<Tokens>())
            //    throw new ArgumentOutOfRangeException(nameof(tokens));

            //this = Unsafe.As<MarkupTokens<T>, Tokens>(ref tokens);
        }
    }

    protected readonly Tokens _tokens;

    protected override int LtLength => 1;

    protected override int LtColonLength => 2;

    protected override int LtSlashLength => 2;

    protected override int LtSlashColonLength => 3;

    public MarkupFinder(int[] codePages, Tokens tokens) : base(codePages)
    {
        _tokens = tokens;
    }

    protected virtual bool IsSpace(T value) => value.Equals(_tokens._space);

    protected virtual bool Equals(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.SequenceEqual(value);

    protected virtual bool IsInvalidNS(T token) =>
        IsSpace(token) ||
        token.Equals(_tokens._gt) ||
        token.Equals(_tokens._colon) ||
        token.Equals(_tokens._eq) ||
        token.Equals(_tokens._quot) ||
        token.Equals(_tokens._apos);

    protected virtual int IndexOfSpace(ReadOnlySpan<T> data)
    {
        for (int i = 0; i < data.Length; i++)
        {
            var token = data[i];
            if (token.Equals(_tokens._gt)) break;
            if (IsSpace(token)) return i;
        }
        return -1;
    }

    //TagName
    protected override int IndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.IndexOf(value);

    //TagName
    protected override int LastIndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.LastIndexOf(value);

    protected override bool IsStartOpening(ReadOnlySpan<T> data, int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start < data.Length);

        return data[start].Equals(_tokens._lt);
    }

    protected override bool IsStartOpening(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + ns.Length + 1 < data.Length);

        return data[start++].Equals(_tokens._lt) &&
               data[start + ns.Length].Equals(_tokens._colon) &&
               Equals(data.Slice(start, ns.Length), ns);
    }

    protected override bool IsStartOpening(ReadOnlySpan<T> data, ref int start, out TagNS ns)
    {
        Debug.Assert(start < data.Length);
        Debug.Assert(start >= 1);

        var token = data[--start];
        if (token.Equals(_tokens._lt))
        {
            ns = default;
            return true;
        }
        else if (token.Equals(_tokens._colon) && start > 1)
        {
            var endNS = start;
            do
            {
                token = data[--start];
                if (token.Equals(_tokens._lt))
                {
                    var startNS = start + 1;
                    if (endNS > startNS)
                    {
                        ns = new(new StartEnd(startNS, endNS));
                        return true;
                    }
                    break;
                }
                else if (token.Equals(_tokens._slash) || IsInvalidNS(token))
                {
                    break;
                }
            } while (start > 0);
        }
        ns = default;
        return false;
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, int start)
    {
        Debug.Assert(start >= 0 && start + 1 < data.Length);

        return data[start++].Equals(_tokens._lt) && data[start].Equals(_tokens._slash);
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0 && start + ns.Length + 2 < data.Length);

        return data[start++].Equals(_tokens._lt) &&
               data[start++].Equals(_tokens._slash) &&
               data[start + ns.Length].Equals(_tokens._colon) &&
               Equals(data.Slice(start, ns.Length), ns);
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, ref int start, out TagNS ns)
    {
        Debug.Assert(start < data.Length);
        Debug.Assert(start >= 2);

        var token = data[--start];

        //</
        if (token.Equals(_tokens._slash))
        {
            ns = default;
            return data[--start].Equals(_tokens._lt);
        }
        //</*:
        else if (token.Equals(_tokens._colon) && start > 2)
        {
            var endNS = start;
            //</
            do
            {
                token = data[--start];
                if (token.Equals(_tokens._slash))
                {
                    if (data[--start].Equals(_tokens._lt))
                    {
                        var startNS = start + 2;
                        if (endNS > startNS)
                        {
                            ns = new(new StartEnd(startNS, endNS));
                            return true;
                        }
                    }
                    break;
                }
                else if (IsInvalidNS(token))
                {
                    break;
                }
            } while (start > 1);
        }
        ns = default;
        return false;
    }

    protected override bool IsEndClosing(ReadOnlySpan<T> data, ref int end, out bool hasSpace)
    {
        Debug.Assert(end >= 0 && end < data.Length);

        hasSpace = false;

        do
        {
            var token = data[end++];
            if (token.Equals(_tokens._gt)) return true;
            if (IsSpace(token))
            {
                hasSpace = true;
                continue;
            }
            break;
        } while (end < data.Length);

        return false;
    }

    protected override TagEnding GetEndingName(ReadOnlySpan<T> data, ref int end)
    {
        Debug.Assert(end >= 0);
        Debug.Assert(end < data.Length);

        var token = data[end++];
        if (token.Equals(_tokens._gt))
            return TagEnding.Closing;

        if (token.Equals(_tokens._slash))
            return end < data.Length && data[end++].Equals(_tokens._gt)
                ? TagEnding.SelfClosing : TagEnding.None;

        if (IsSpace(token))
            return TagEnding.Name;

        return TagEnding.None;
    }

    protected override TagEnding GetEndingAttributeStart(ReadOnlySpan<T> data, ref int end)
    {
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            var token = data[end];
            if (token.Equals(_tokens._gt))
            {
                end++;
                return TagEnding.Closing;
            }
            else if (token.Equals(_tokens._slash))
            {
                end++;
                return end < data.Length && data[end++].Equals(_tokens._gt)
                    ? TagEnding.SelfClosing : TagEnding.None;
            }
            else if (IsSpace(token))
            {
                end++;
            }
            else
            {
                return TagEnding.AttributeStart;
            }
        }

        return TagEnding.None;
    }

    protected override TagEnding GetEndingHasAttributes(ReadOnlySpan<T> data, ref int end)
    {
        Debug.Assert(end < data.Length);
        Debug.Assert(end >= 0);

        //TODO: _gt, _slash и space был проверен ранее
        //стоит пропустить этот символ?
        //Debug.Assert(!data[end].Equals(_tokens._gt));
        //Debug.Assert(!data[end].Equals(_tokens._slash));

        do
        {
            var token = data[end++];
            if (token.Equals(_tokens._gt))
            {
                return TagEnding.ClosingHasAttributes;
            }
            else if (token.Equals(_tokens._slash))
            {
                return end < data.Length && data[end++].Equals(_tokens._gt) ? TagEnding.SelfClosingHasAttributes : TagEnding.None;
            }
            else if (token.Equals(_tokens._quot))
            {
                //Добавляем еще длину gt
                if (end + 1 >= data.Length) break;

                var index = data.Slice(end).IndexOf(_tokens._quot);
                if (index < 0) break;
                end += index + 1;
            }
            else if (token.Equals(_tokens._apos))
            {
                //Добавляем еще длину gt
                if (end + 1 >= data.Length) break;

                var index = data.Slice(end).IndexOf(_tokens._apos);
                if (index < 0) break;
                end += index + 1;
            }
        } while (end < data.Length);

        return TagEnding.None;
    }

    protected override Tag FirstTagByAttribute(ReadOnlySpan<T> data, ReadOnlySpan<T> value, IAttName name, out TagNS tagName)
    {
        var valen = value.Length;
        Debug.Assert(valen > 0);

        //<a b=
        const int min = 5;
        int offset = 0;
        do
        {
#if DEBUG && NET
            var str = Info.ToString(data.Slice(offset));
#endif
            //case sensitive always
            var index = data.Slice(offset).IndexOf(value);
            if (index < 0) break;

            index = checked(index + offset);

            //ищем tagName до пробела
            offset = index + valen;
            if (index >= min)
            {
                //-check quotes "" or apos '' (если значение не имеет пробелов, то может быть без ковычек)
                if (offset + 1 < data.Length && IsQuoted(data[index - 1], data[offset]))
                {
                    index--;
                    tagName = GetTagName(data.Slice(0, index));
                    if (!tagName.IsEmpty)
                    {
                        index -= tagName.End + 1;
                        var attrName = GetAttrName(data.Slice(tagName.End + 1, index));
                        if (!attrName.IsEmpty)
                        {
#if DEBUG && NET
                            var tagNameStr = Info.ToString(data.Slice(tagName.Start, tagName.Length));
                            var attNameStr = Info.ToString(attrName);
#endif
                            //TODO: replace data to dtd
                            if (name.Equals(data.Slice(tagName.Start, tagName.Length), attrName, data))
                            {
                                offset++;
                                var ending = GetEndingHasAttributes(data, ref offset);
                                if (ending != TagEnding.None)
                                {
                                    return new(tagName.Start - 1, offset, ending);
                                }
                            }
                        }
                    }
                }
            }
        } while (true);

        tagName = default;
        return default;
    }

    private bool IsQuoted(T first, T last) =>
        first.Equals(_tokens._quot) && last.Equals(_tokens._quot) ||
        first.Equals(_tokens._apos) && last.Equals(_tokens._apos);

    private TagNS GetTagName(ReadOnlySpan<T> data)
    {
#if DEBUG && NET
        var str = Info.ToString(data);
#endif
        //find LT
        var tagNameStart = data.LastIndexOf(_tokens._lt);
        if (tagNameStart < 0) return default;

        tagNameStart++;
#if DEBUG && NET
        str = Info.ToString(data.Slice(tagNameStart));
#endif
        //find any space
        var tagNameLength = IndexOfSpace(data.Slice(tagNameStart));
        if (tagNameLength < 1) return default;//imposible?

#if DEBUG && NET
        str = Info.ToString(data.Slice(tagNameStart, tagNameLength));
#endif

        return new(new(tagNameStart, tagNameLength + tagNameStart));
    }

    private ReadOnlySpan<T> GetAttrName(ReadOnlySpan<T> data)
    {
#if DEBUG && NET
        var str = Info.ToString(data);
#endif
        var nameStart = 0;
        var nameLength = 0;
        bool isNew = true;
        for (int i = 0; i < data.Length; i++)
        {
            var token = data[i];
            if (token.Equals(_tokens._gt) || token.Equals(_tokens._slash))
            {
                return default;
            }
            else if (token.Equals(_tokens._eq))
            {
                if (nameLength == 0)
                    return default;

                var state = ReadAttrValue(data, ref i);
                if (state == AttrValueState.End)
                    return data.Slice(nameStart, nameLength);
                
                if (state == AttrValueState.Invalid)
                    return default;

                isNew = true;
            }
            else if (IsSpace(token))
            {
                isNew = true;
            }
            else if (isNew)
            {
                isNew = false;
                nameStart = i;
                nameLength = 1;
            }
            else
            {
                nameLength++;
            }
        }

        return default;
    }

    private AttrValueState ReadAttrValue(ReadOnlySpan<T> data, ref int i)
    {
#if DEBUG && NET
        var str = Info.ToString(data.Slice(i));
#endif
        int length = 0;
        while (++i < data.Length)
        {
            var token = data[i];
            if (token.Equals(_tokens._gt) || token.Equals(_tokens._slash) || token.Equals(_tokens._eq))
            {
                return AttrValueState.Invalid;
            }
            else if (token.Equals(_tokens._quot))
            {
                i++;
                if (i >= data.Length) return AttrValueState.Invalid;

                var index = data.Slice(i).IndexOf(_tokens._quot);
                if (index < 0) return AttrValueState.Invalid;
                i += index;

                return AttrValueState.Read;
            }
            else if (token.Equals(_tokens._apos))
            {
                i++;
                if (i >= data.Length) return AttrValueState.Invalid;

                var index = data.Slice(i).IndexOf(_tokens._apos);
                if (index < 0) return AttrValueState.Invalid;
                i += index;

                return AttrValueState.Read;
            }
            else if (IsSpace(token))
            {
                if (length > 0)
                    return AttrValueState.Read;
            }
            else
            {
                length++;
            }
        }
        return AttrValueState.End;
    }

    enum AttrValueState
    {
        Read = 0,
        Invalid,
        End,
    }
}