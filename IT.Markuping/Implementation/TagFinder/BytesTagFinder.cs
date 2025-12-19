using IT.Markuping.Encodings;
using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public class BytesTagFinder : ITagFinder<byte>
{
    private readonly byte[] _lt;
    private readonly byte[] _gt;
    private readonly byte[] _slash;
    private readonly byte[] _colon;
    private readonly byte[] _space;
    private readonly byte[] _quot;
    private readonly byte[] _apos;
    private readonly byte[] _eq;
    private readonly byte[][] _otherSpaces;
    private readonly byte[] _startClosing;
    private readonly byte[] _selfClosing;
    private readonly int _minLength;

    public static readonly BytesTagFinder Utf16 = new(BytesEncoding.Utf16);
    public static readonly BytesTagFinder Utf16BE = new(BytesEncoding.Utf16BE);
    public static readonly BytesTagFinder Utf32 = new(BytesEncoding.Utf32);
    public static readonly BytesTagFinder Utf32BE = new(BytesEncoding.Utf32BE);

    public BytesTagFinder(BytesEncoding bytesEncoding)
    {
        if (bytesEncoding == null) throw new ArgumentNullException(nameof(bytesEncoding));

        _lt = bytesEncoding._lt;
        _gt = bytesEncoding._gt;
        _slash = bytesEncoding._slash;
        _colon = bytesEncoding._colon;
        _space = bytesEncoding._space;
        _quot = bytesEncoding._quot;
        _apos = bytesEncoding._apos;
        _eq = bytesEncoding._eq;
        _otherSpaces = bytesEncoding._otherSpaces;
        _startClosing = bytesEncoding._startClosing;
        _selfClosing = bytesEncoding._selfClosing;
        _minLength = bytesEncoding._minLength;
    }

    #region ITagFinder

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
            if (index >= _lt.Length)
            {
                var tag = GetTag(data, index - _lt.Length, end, endings);
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
            if (index < _lt.Length) break;

            var tag = GetTag(data, index - _lt.Length, index + namelen, endings);
            if (!tag.IsEmpty)
            {
                return tag;
            }

            data = data.Slice(0, index);
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
            if (index < _startClosing.Length) break;

            var closing = GetClosing(data, index, index + namelen, out ns);
            if (!closing.IsEmpty)
            {
                return closing;
            }

            data = data.Slice(0, index);
        } while (true);

        ns = default;
        return default;
    }

    #endregion

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
        var min = nslen + _lt.Length + _colon.Length;
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

        return default;
    }

    private Tag LastNS(ReadOnlySpan<byte> data, ReadOnlySpan<byte> name, ReadOnlySpan<byte> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + _lt.Length + _colon.Length;
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

        return default;
    }

    private Tag GetTag(ReadOnlySpan<byte> data, int start, int end, TagEndings endings)
    {
        Debug.Assert(start >= 0 && start + _lt.Length <= data.Length);
        Debug.Assert(end > 0 && start < end);

        if (data.Slice(start, _lt.Length).SequenceEqual(_lt))
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
        Debug.Assert(start + _lt.Length + ns.Length + _colon.Length < data.Length);

        if (!data.Slice(start, _lt.Length).SequenceEqual(_lt)) return false;

        start += _lt.Length;

        if (!data.Slice(start, ns.Length).SequenceEqual(ns)) return false;

        start += ns.Length;

        return data.Slice(start, _colon.Length).SequenceEqual(_colon);
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
        Debug.Assert(start < data.Length);
        Debug.Assert(start >= _startClosing.Length);

        if (data.Slice(start - _startClosing.Length, _startClosing.Length).SequenceEqual(_startClosing))
        {
            start -= _startClosing.Length;
            ns = default;
            return true;
        }

        start -= _colon.Length;
        if (start >= _startClosing.Length && data.Slice(start, _colon.Length).SequenceEqual(_colon))
        {
            var endNS = start;
            do
            {
                if (data.Slice(start - _startClosing.Length, _startClosing.Length).SequenceEqual(_startClosing))
                {
                    ns = start..endNS;
                    start -= _startClosing.Length;
                    return true;
                }
                else if (start >= _quot.Length && data.Slice(start - _quot.Length, _quot.Length).SequenceEqual(_quot))
                {
                    break;
                }
                else if (start >= _apos.Length && data.Slice(start - _apos.Length, _apos.Length).SequenceEqual(_apos))
                {
                    break;
                }
                else
                {
                    //TODO: спорное решение для байтов переменной длины
                    start -= _minLength;
                }
            } while (start >= _startClosing.Length);
        }
        ns = default;
        return false;
    }

    private bool IsEndClosing(ReadOnlySpan<byte> data, ref int end, out bool hasSpace)
    {
        Debug.Assert(end >= 0);

        hasSpace = false;

        while (end < data.Length)
        {
            if (IsSeq(data, _gt, ref end)) return true;
            if (IsSeq(data, _space, ref end) || IsOtherSpace(data, ref end))
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

        if (IsSeq(data, _gt, ref end))
            return TagEnding.Closing;

        if (IsSeq(data, _selfClosing, ref end))
            return TagEnding.SelfClosing;

        if (IsSeq(data, _space, ref end) || IsOtherSpace(data, ref end))
            return TagEnding.Name;

        return TagEnding.None;
    }

    private TagEnding GetEndingAttributeStart(ReadOnlySpan<byte> data, ref int end)
    {
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            if (IsSeq(data, _gt, ref end))
            {
                return TagEnding.Closing;
            }
            else if (IsSeq(data, _selfClosing, ref end))
            {
                return TagEnding.SelfClosing;
            }
            else if (IsSeq(data, _space, ref end) || IsOtherSpace(data, ref end))
            {
                continue;
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
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            if (IsSeq(data, _gt, ref end))
            {
                return TagEnding.ClosingHasAttributes;
            }
            else if (IsSeq(data, _selfClosing, ref end))
            {
                return TagEnding.SelfClosingHasAttributes;
            }
            else if (IsSeq(data, _quot, ref end))
            {
                if (end + _quot.Length >= data.Length) break;

                var index = data.Slice(end).IndexOf(_quot);
                if (index < 0) break;
                end += index + _quot.Length;
            }
            else if (IsSeq(data, _apos, ref end))
            {
                if (end + _apos.Length >= data.Length) break;

                var index = data.Slice(end).IndexOf(_apos);
                if (index < 0) break;
                end += index + _apos.Length;
            }
            else
            {
                //TODO: спорное решение для байтов переменной длины
                end += _minLength;
            }
        }

        return TagEnding.None;
    }

    private static bool IsSeq(ReadOnlySpan<byte> data, ReadOnlySpan<byte> value, ref int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(value.Length > 0);

        if (start + value.Length <= data.Length && data.Slice(start, value.Length).SequenceEqual(value))
        {
            start += value.Length;
            return true;
        }
        return false;
    }

    private bool IsOtherSpace(ReadOnlySpan<byte> span, ref int end)
    {
        var len = span.Length - end;
        foreach (var otherSpace in _otherSpaces)
        {
            if (len >= otherSpace.Length && span.Slice(end, otherSpace.Length).SequenceEqual(otherSpace))
            {
                end += otherSpace.Length;
                return true;
            }
        }
        return false;
    }

    #endregion Private Methods

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out BytesTagFinder bytesTagFinder)
    {
        if (codePage == 1200)
        {
            bytesTagFinder = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            bytesTagFinder = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            bytesTagFinder = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            bytesTagFinder = Utf32BE;
            return true;
        }
        bytesTagFinder = null;
        return false;
    }
}