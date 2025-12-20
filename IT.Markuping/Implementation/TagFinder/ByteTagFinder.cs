using IT.Markuping.Encodings;
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public class ByteTagFinder : ITagFinder<byte>
{
    private readonly bool[]? _otherSpaces;
    private readonly ByteEncoding.Tokens _tokens;

    public static readonly ByteTagFinder Utf8 = new(ByteEncoding.Utf8);
    public static readonly ByteTagFinder Europa = new(ByteEncoding.Europa);
    public static readonly ByteTagFinder EBCDIC = new(ByteEncoding.EBCDIC);
    public static readonly ByteTagFinder EBCDIC_Turkish = new(ByteEncoding.EBCDIC_Turkish);
    public static readonly ByteTagFinder IBM_Latin1 = new(ByteEncoding.IBM_Latin1);

    public ByteTagFinder(ByteEncoding.Tokens tokens, bool[]? otherSpaces)
    {
        _otherSpaces = otherSpaces;
        _tokens = tokens;
    }

    public ByteTagFinder(ByteEncoding byteEncoding)
    {
        if (byteEncoding == null) throw new ArgumentNullException(nameof(byteEncoding));

        _otherSpaces = byteEncoding._otherSpaces;
        _tokens = byteEncoding._tokens;
    }

    #region ITagFinder

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        var opening = First(data, name, ns, TagEndings.Closing);
        if (!opening.IsEmpty)
        {
            var closing = FirstClosing(data.Slice(opening.End), name, ns);
            if (!closing.IsEmpty)
            {
                return new((TagOpening)opening, closing.AddOffset(opening.End));
            }
        }
        return default;
    }

    public Tags FirstPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        var opening = First(data, name, TagEndings.Closing);
        if (!opening.IsEmpty)
        {
            var closing = FirstClosing(data.Slice(opening.End), name);
            if (!closing.IsEmpty)
            {
                return new((TagOpening)opening, closing.AddOffset(opening.End));
            }
        }
        return default;
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var closing = LastClosing(data, name, out ns);
        if (!closing.IsEmpty)
        {
            var opening = Last(data.Slice(0, closing.Start), name, data[ns], TagEndings.Closing);
            if (!opening.IsEmpty)
            {
                return new((TagOpening)opening, closing);
            }
        }
        ns = default;
        return default;
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        var closing = LastClosing(data, name, ns);
        if (!closing.IsEmpty)
        {
            var opening = Last(data.Slice(0, closing.Start), name, ns, TagEndings.Closing);
            if (!opening.IsEmpty)
            {
                return new((TagOpening)opening, closing);
            }
        }
        return default;
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        var closing = LastClosing(data, name);
        if (!closing.IsEmpty)
        {
            var opening = Last(data.Slice(0, closing.Start), name, TagEndings.Closing);
            if (!opening.IsEmpty)
            {
                return new((TagOpening)opening, closing);
            }
        }
        return default;
    }

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = default)
        => ns.IsEmpty ? First(data, name, endings) : FirstNS(data, name, ns, endings);

    public Tag First(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var len = data.Length;
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        do
        {
            var index = data.IndexOf(name);
            if (index < 0) break;

            var end = index + namelen;
            if (index > 0)
            {
                var tag = GetTag(data, index - 1, end, endings);
                if (!tag.IsEmpty)
                {
                    return tag.AddOffset(len - data.Length);
                }
            }

            data = data.Slice(end);
        } while (true);

        return default;
    }

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = default)
        => ns.IsEmpty ? Last(data, name, endings) : LastNS(data, name, ns, endings);

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        do
        {
            var index = data.LastIndexOf(name);
            if (index < 1) break;

            var tag = GetTag(data, index - 1, index + namelen, endings);
            if (!tag.IsEmpty)
            {
                return tag;
            }

            data = data.Slice(0, index);
        } while (true);

        return default;
    }

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => ns.IsEmpty ? FirstClosing(data, name) : FirstClosingNS(data, name, ns);

    public TagClosing FirstClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var len = data.Length;
        do
        {
            var index = data.IndexOf(name);
            if (index < 0) break;

            var end = index + namelen;
            if (index > 1)
            {
                var closing = GetClosing(data, index - 2, end);
                if (!closing.IsEmpty)
                {
                    return closing.AddOffset(len - data.Length);
                }
            }
            data = data.Slice(end);
        } while (true);

        return default;
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        do
        {
            var index = data.LastIndexOf(name);
            if (index < 2) break;

            var closing = GetClosing(data, index - 1, index + namelen, out ns);
            if (!closing.IsEmpty)
            {
                return closing;
            }

            data = data.Slice(0, index);
        } while (true);

        ns = default;
        return default;
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
        => ns.IsEmpty ? LastClosing(data, name) : LastClosingNS(data, name, ns);

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        do
        {
            var index = data.LastIndexOf(name);
            if (index < 2) break;

            var closing = GetClosing(data, index - 2, index + namelen);
            if (!closing.IsEmpty)
            {
                return closing;
            }

            data = data.Slice(0, index);
        } while (true);

        return default;
    }

    #endregion ITagFinder

    #region Private Methods

    private Tag FirstNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var len = data.Length;
        //"<ns:"
        var min = nslen + 2;
        do
        {
            var index = data.IndexOf(name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var tag = GetTag(data, index - min, end, endings, ns);
                if (!tag.IsEmpty)
                {
                    return tag.AddOffset(len - data.Length);
                }
            }

            data = data.Slice(end);
        } while (true);

        return default;
    }

    private Tag LastNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + 2;
        do
        {
            var index = data.LastIndexOf(name);
            if (index < min) break;

            var tag = GetTag(data, index - min, index + namelen, endings, ns);
            if (!tag.IsEmpty)
            {
                return tag;
            }

            data = data.Slice(0, index);
        } while (true);

        return default;
    }

    private TagClosing FirstClosingNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var len = data.Length;
        var min = nslen + 3;//</ns:
        do
        {
            var index = data.IndexOf(name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var closing = GetClosing(data, index - min, end, ns);
                if (!closing.IsEmpty)
                {
                    return closing.AddOffset(len - data.Length);
                }
            }
            data = data.Slice(end);
        } while (true);

        return default;
    }

    private TagClosing LastClosingNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns)
    {
        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + 3;//</ns:
        do
        {
            var index = data.LastIndexOf(name);
            if (index < min) break;

            var closing = GetClosing(data, index - min, index + namelen, ns);
            if (!closing.IsEmpty)
            {
                return closing;
            }

            data = data.Slice(0, index);
        } while (true);

        return default;
    }

    private Tag GetTag(ReadOnlySpan<byte> data, int start, int end, TagEndings endings)
    {
        Debug.Assert(start >= 0 && start < data.Length);
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && data[start] == _tokens._lt)
        {
            var ending = endings.IsAnyClosing() ?
                GetEndingAnyClosing(data, ref end) :
                GetEnding(data, ref end, endings);
            if (ending != TagEnding.None)
            {
                return new(start, end, ending);
            }
        }
        return default;
    }

    private Tag GetTag(ReadOnlySpan<byte> data, int start, int end, TagEndings endings, ReadOnlySpan<byte> ns)
    {
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && IsStartOpening(data, start, ns))
        {
            var ending = endings.IsAnyClosing() ?
                GetEndingAnyClosing(data, ref end) :
                GetEnding(data, ref end, endings);
            if (ending != TagEnding.None)
            {
                return new(start, end, ending);
            }
        }
        return default;
    }

    internal bool IsStartOpening(ReadOnlySpan<byte> data, int start, ReadOnlySpan<byte> ns)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + ns.Length + 1 < data.Length);

        return data[start++] == _tokens._lt &&
               data[start + ns.Length] == _tokens._colon &&
               data.Slice(start, ns.Length).SequenceEqual(ns);
    }

    private TagClosing GetClosing(ReadOnlySpan<byte> data, int start, int end, out Range ns)
    {
        Debug.Assert(start > 0 && end > 0);
        Debug.Assert(start < end);

        if (IsEndClosing(data, ref end, out var hasSpace) &&
            IsStartClosing(data, ref start, out ns))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        ns = default;
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<byte> data, int start, int end)
    {
        Debug.Assert(start >= 0 && start + 1 < data.Length);
        Debug.Assert(end > 0 && start < end);

        if (IsStartClosing(data, start) &&
            IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<byte> data, int start, int end, ReadOnlySpan<byte> ns)
    {
        Debug.Assert(start >= 0 && end > 0);
        Debug.Assert(start < end);

        if (IsStartClosing(data, start, ns) &&
            IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    internal bool IsStartClosing(ReadOnlySpan<byte> data, int start)
    {
        Debug.Assert(start >= 0 && start + 1 < data.Length);

        return data[start++] == _tokens._lt && data[start] == _tokens._slash;
    }

    internal bool IsStartClosing(ReadOnlySpan<byte> data, int start, ReadOnlySpan<byte> ns)
    {
        Debug.Assert(start >= 0 && start + ns.Length + 2 < data.Length);

        return data[start++] == _tokens._lt &&
               data[start++] == _tokens._slash &&
               data[start + ns.Length] == _tokens._colon &&
               data.Slice(start, ns.Length).SequenceEqual(ns);
    }

    internal bool IsStartClosing(ReadOnlySpan<byte> data, ref int start, out Range ns)
    {
        Debug.Assert(start < data.Length);
        Debug.Assert(start > 0);

        var token = data[start];

        //</
        if (token == _tokens._slash)
        {
            ns = default;
            return data[--start] == _tokens._lt;
        }
        //</*:
        else if (token == _tokens._colon && start > 2)
        {
            var endNS = start;
            //</
            do
            {
                token = data[--start];
                if (token == _tokens._slash)
                {
                    if (data[--start] == _tokens._lt)
                    {
                        ns = (start + 2)..endNS;
                        return true;
                    }
                    break;
                }
                else if (token == _tokens._quot || token == _tokens._apos)
                {
                    break;
                }
            } while (start > 1);
        }
        ns = default;
        return false;
    }

    internal bool IsEndClosing(ReadOnlySpan<byte> data, ref int end, out bool hasSpace)
    {
        Debug.Assert(end >= 0);

        hasSpace = false;

        while (end < data.Length)
        {
            var token = data[end++];
            if (token == _tokens._gt) return true;
            if (token == _tokens._space || _otherSpaces != null && _otherSpaces[token])
            {
                hasSpace = true;
                continue;
            }
            break;
        }

        return false;
    }

    private TagEnding GetEndingAnyClosing(ReadOnlySpan<byte> data, ref int end)
    {
        var ending = GetEndingName(data, ref end);
        if (ending == TagEnding.Name)
        {
            ending = GetEndingAttributeStart(data, ref end);
            if (ending == TagEnding.AttributeStart)
                ending = GetEndingHasAttributes(data, ref end);
        }

        return ending;
    }

    private TagEnding GetEnding(ReadOnlySpan<byte> data, ref int end, TagEndings endings)
    {
        var ending = GetEndingName(data, ref end);
        if (endings == TagEndings.HasNoAttributes)
            return ending;

        var hasAttributes = endings.HasAttributes();
        var hasNoAttributes = endings.HasNoAttributes();

        if (ending == TagEnding.Name)
        {
            ending = GetEndingAttributeStart(data, ref end);
            if (endings == TagEndings.HasAttributes)
                return ending == TagEnding.AttributeStart ? TagEnding.AttributeStart : TagEnding.None;

            if (ending == TagEnding.AttributeStart && (hasAttributes || !hasNoAttributes))
            {
                ending = GetEndingHasAttributes(data, ref end);
                if (ending == TagEnding.ClosingHasAttributes)
                {
                    if (endings.HasClosing())
                        return TagEnding.ClosingHasAttributes;
                }
                else if (ending == TagEnding.SelfClosingHasAttributes)
                {
                    if (endings.HasSelfClosing())
                        return TagEnding.SelfClosingHasAttributes;
                }

                return TagEnding.None;
            }
        }

        if (hasNoAttributes || !hasAttributes)
        {
            if (ending == TagEnding.Closing)
            {
                if (endings.HasClosing())
                    return TagEnding.Closing;
            }
            else if (ending == TagEnding.SelfClosing)
            {
                if (endings.HasSelfClosing())
                    return TagEnding.SelfClosing;
            }
        }

        return TagEnding.None;
    }

    private TagEnding GetEndingName(ReadOnlySpan<byte> data, ref int end)
    {
        Debug.Assert(end >= 0);
        Debug.Assert(end < data.Length);

        var token = data[end++];
        if (token == _tokens._gt)
            return TagEnding.Closing;

        if (token == _tokens._slash)
            return end < data.Length && data[end++] == _tokens._gt
                ? TagEnding.SelfClosing : TagEnding.None;

        if (token == _tokens._space || _otherSpaces != null && _otherSpaces[token])
            return TagEnding.Name;

        return TagEnding.None;
    }

    private TagEnding GetEndingAttributeStart(ReadOnlySpan<byte> data, ref int end)
    {
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            var token = data[end];
            if (token == _tokens._gt)
            {
                end++;
                return TagEnding.Closing;
            }
            else if (token == _tokens._slash)
            {
                end++;
                return end < data.Length && data[end++] == _tokens._gt
                    ? TagEnding.SelfClosing : TagEnding.None;
            }
            else if (token == _tokens._space || _otherSpaces != null && _otherSpaces[token])
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

    private TagEnding GetEndingHasAttributes(ReadOnlySpan<byte> data, ref int end)
    {
        Debug.Assert(end < data.Length);
        Debug.Assert(end >= 0);

        //TODO: _gt, _slash и space был проверен ранее
        //стоит пропустить этот символ?
        Debug.Assert(data[end] != _tokens._gt);
        Debug.Assert(data[end] != _tokens._slash);

        do
        {
            var token = data[end++];
            if (token == _tokens._gt)
            {
                return TagEnding.ClosingHasAttributes;
            }
            else if (token == _tokens._slash)
            {
                return end < data.Length && data[end++] == _tokens._gt ? TagEnding.SelfClosingHasAttributes : TagEnding.None;
            }
            else if (token == _tokens._quot)
            {
                //Добавляем еще длину gt
                if (end + 1 >= data.Length) break;

                var index = data.Slice(end).IndexOf(_tokens._quot);
                if (index < 0) break;
                end += index + 1;
            }
            else if (token == _tokens._apos)
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

    #endregion Private Methods

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ByteTagFinder byteTagFinder)
    {
        if (Array.IndexOf(ByteEncoding.Utf8_CodePages, codePage) > -1)
        {
            byteTagFinder = Utf8;
            return true;
        }
        if (codePage == 29001)
        {
            byteTagFinder = Europa;
            return true;
        }
        if (Array.IndexOf(ByteEncoding.EBCDIC_CodePages, codePage) > -1)
        {
            byteTagFinder = EBCDIC;
            return true;
        }
        if (codePage == 1026 || codePage == 20905)
        {
            byteTagFinder = EBCDIC_Turkish;
            return true;
        }
        if (codePage == 1047 || codePage == 20924)
        {
            byteTagFinder = IBM_Latin1;
            return true;
        }
        byteTagFinder = null;
        return false;
    }
}