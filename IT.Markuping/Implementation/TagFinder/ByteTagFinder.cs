using IT.Markuping.Encodings;
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

public class ByteTagFinder : ITagFinder<byte>
{
    private readonly byte[] _startClosing;
    private readonly bool[]? _otherSpaces;
    private readonly ByteEncoding.Tokens _tokens;

    public ByteTagFinder(ByteEncoding byteEncoding)
    {
        if (byteEncoding == null) throw new ArgumentNullException(nameof(byteEncoding));

        _startClosing = byteEncoding._startClosing;
        _otherSpaces = byteEncoding._otherSpaces;
        _tokens = byteEncoding._tokens;
    }

    #region ITagFinder

    public Tags WrapPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var namelen = name.Length;
        if (data.Length >= (namelen * 2) + 5)
        {
            var closing = LastClosing(data, name, out ns);
            if (closing.Start >= namelen + 2)
            {
                var opening = First(data.Slice(0, closing.Start), name, data[ns], TagEndings.Closing);
                if (!opening.IsEmpty)
                {
                    return new((TagOpening)opening, closing);
                }
            }
        }
        ns = default;
        return default;
    }

    public Tags LastPair(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var namelen = name.Length;
        if (data.Length >= (namelen * 2) + 5)
        {
            var closing = LastClosing(data, name, out ns);
            if (closing.Start >= namelen + 2)
            {
                var opening = Last(data.Slice(0, closing.Start), name, data[ns], TagEndings.Closing);
                if (!opening.IsEmpty)
                {
                    return new((TagOpening)opening, closing);
                }
            }
        }
        ns = default;
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

        if (len >= namelen + 2)
        {
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
        }

        return default;
    }

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings = default)
        => ns.IsEmpty ? Last(data, name, endings) : LastNS(data, name, ns, endings);

    public Tag Last(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        //"<Tag>"
        //"<Tag "
        if (data.Length >= namelen + 2)
        {
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
        }
        return default;
    }

    public TagClosing LastClosing(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, out Range ns)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        if (data.Length >= namelen + 3)
        {
            do
            {
                //"Tag"
                var index = data.LastIndexOf(name);
                if (index < 2) break;

                var closing = GetClosing(data, index - 1, index + namelen, out ns);
                if (!closing.IsEmpty)
                {
                    return closing;
                }

                data = data.Slice(0, index);
            } while (true);
        }
        ns = default;
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
        if (len >= namelen + nslen + 3)
        {
            //"<ns:"
            var min = nslen + 2;
            do
            {
                var index = data.IndexOf(name);
                if (index < 0) break;

                var end = index + namelen;
                if (index >= min)
                {
                    var tag = GetTag(data, ns, index - min, end, endings);
                    if (!tag.IsEmpty)
                    {
                        return tag.AddOffset(len - data.Length);
                    }
                }

                data = data.Slice(end);
            } while (true);
        }

        return default;
    }

    private Tag LastNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        //Example1: "<ns:Tag "
        //Example3: "<ns:Tag>"
        if (data.Length >= namelen + nslen + 3)
        {
            //"<ns:"
            var min = nslen + 2;
            do
            {
                var index = data.LastIndexOf(name);
                if (index < min) break;

                var tag = GetTag(data, ns, index - min, index + namelen, endings);
                if (!tag.IsEmpty)
                {
                    return tag;
                }

                data = data.Slice(0, index);
            } while (true);
        }

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

    private Tag GetTag(ReadOnlySpan<byte> data, ReadOnlySpan<byte> ns, int start, int end, TagEndings endings)
    {
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && IsStartOpening(data, ns, start))
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

    private bool IsStartOpening(ReadOnlySpan<byte> data, ReadOnlySpan<byte> ns, int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + ns.Length + 2 < data.Length);

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
            return new(start, end,
                hasNamespace: ns.Start.Value != ns.End.Value,
                hasSpace: hasSpace);
        }
        ns = default;
        return default;
    }

    private bool IsStartClosing(ReadOnlySpan<byte> data, ref int start, out Range ns)
    {
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
            start = data.Slice(0, endNS).LastIndexOf(_startClosing);
            if (start > -1)
            {
                ns = (start + 2)..endNS;
                return true;
            }
        }
        ns = default;
        return false;
    }

    private bool IsEndClosing(ReadOnlySpan<byte> data, ref int end, out bool hasSpace)
    {
        Debug.Assert(end >= 0);

        hasSpace = false;

        if (_otherSpaces == null)
        {
            while (end < data.Length)
            {
                var token = data[end++];
                if (token == _tokens._gt) return true;
                if (token != _tokens._space) break;
                hasSpace = true;
            }
        }
        else
        {
            while (end < data.Length)
            {
                var token = data[end++];
                if (token == _tokens._gt) return true;
                if (token != _tokens._space && !_otherSpaces[token]) break;
                hasSpace = true;
            }
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
                return ending;

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
            else if (token != _tokens._space && (_otherSpaces == null || !_otherSpaces[token]))
            {
                return TagEnding.AttributeStart;
            }
            else
            {
                end++;
            }
        }

        return TagEnding.None;
    }

    private TagEnding GetEndingHasAttributes(ReadOnlySpan<byte> data, ref int end)
    {
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            var token = data[end++];
            if (token == _tokens._gt)
            {
                return TagEnding.ClosingHasAttributes;
            }
            else if (token == _tokens._slash)
            {
                if (end >= data.Length) break;

                if (data[end++] == _tokens._gt)
                    return TagEnding.SelfClosingHasAttributes;
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
        }

        return TagEnding.None;
    }

    #endregion Private Methods
}