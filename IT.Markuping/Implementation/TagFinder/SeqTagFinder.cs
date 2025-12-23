using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

public class SeqTagFinder<T> : TagFinder<T> where T : unmanaged, IEquatable<T>
{
    private readonly T[] _lt;
    private readonly T[] _gt;
    private readonly T[] _slash;
    private readonly T[] _colon;
    private readonly T[] _space;
    private readonly T[] _quot;
    private readonly T[] _apos;
    private readonly T[] _eq;
    private readonly T[][] _otherSpaces;
    private readonly int _minLength;

    protected override int LtLength => _lt.Length;

    protected override int LtColonLength => _lt.Length + _colon.Length;

    protected override int LtSlashLength => _lt.Length + _slash.Length;

    protected override int LtSlashColonLength => _lt.Length + _slash.Length + _colon.Length;

    public SeqTagFinder(int length, T[] lt, T[] gt, T[] slash, T[] colon,
        T[] space, T[] quot, T[] apos, T[] eq, T[][] otherSpaces)
    {
        _minLength = length;
        _lt = lt;
        _gt = gt;
        _slash = slash;
        _colon = colon;
        _space = space;
        _quot = quot;
        _apos = apos;
        _eq = eq;
        _otherSpaces = otherSpaces;
    }

    protected override int IndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.IndexOf(value);

    protected override int LastIndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.LastIndexOf(value);

    protected override bool IsStartOpening(ReadOnlySpan<T> data, int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + _lt.Length <= data.Length);

        return data.Slice(start, _lt.Length).SequenceEqual(_lt);
    }

    protected override bool IsStartOpening(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + _lt.Length + ns.Length + _colon.Length <= data.Length);

        if (!data.Slice(start, _lt.Length).SequenceEqual(_lt)) return false;

        start += _lt.Length;

        if (!data.Slice(start, ns.Length).SequenceEqual(ns)) return false;

        start += ns.Length;

        return data.Slice(start, _colon.Length).SequenceEqual(_colon);
    }

    protected override bool IsStartOpening(ReadOnlySpan<T> data, ref int start, out Range ns)
    {
        Debug.Assert(start < data.Length);
        Debug.Assert(start >= _lt.Length);

        if (data.Slice(start - _lt.Length, _lt.Length).SequenceEqual(_lt))
        {
            start -= _lt.Length;
            ns = default;
            return true;
        }

        start -= _colon.Length;
        if (start > _lt.Length && data.Slice(start, _colon.Length).SequenceEqual(_colon))
        {
            var endNS = start;
            do
            {
                if (data.Slice(start - _lt.Length, _lt.Length).SequenceEqual(_lt))
                {
                    Debug.Assert(endNS > start);

                    ns = start..endNS;
                    start -= _lt.Length;
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
            } while (start >= _lt.Length);
        }
        ns = default;
        return false;
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + _lt.Length + _slash.Length <= data.Length);

        return data.Slice(start, _lt.Length).SequenceEqual(_lt) &&
               data.Slice(start + _lt.Length, _slash.Length).SequenceEqual(_slash);
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(start + _lt.Length + _slash.Length + ns.Length + _colon.Length <= data.Length);

        if (!data.Slice(start, _lt.Length).SequenceEqual(_lt)) return false;

        start += _lt.Length;

        if (!data.Slice(start, _slash.Length).SequenceEqual(_slash)) return false;

        start += _slash.Length;

        if (!data.Slice(start, ns.Length).SequenceEqual(ns)) return false;

        start += ns.Length;

        return data.Slice(start, _colon.Length).SequenceEqual(_colon);
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, ref int start, out Range ns)
    {
        var startClosingLength = _lt.Length + _slash.Length;

        Debug.Assert(start < data.Length);
        Debug.Assert(start >= startClosingLength);

        if (data.Slice(start - _slash.Length, _slash.Length).SequenceEqual(_slash))
        {
            start -= startClosingLength;
            ns = default;
            return data.Slice(start, _lt.Length).SequenceEqual(_lt);
        }

        start -= _colon.Length;
        if (start > startClosingLength && data.Slice(start, _colon.Length).SequenceEqual(_colon))
        {
            var endNS = start;
            do
            {
                if (data.Slice(start - _slash.Length, _slash.Length).SequenceEqual(_slash))
                {
                    if (data.Slice(start - startClosingLength, _lt.Length).SequenceEqual(_lt))
                    {
                        Debug.Assert(endNS > start);

                        ns = start..endNS;
                        start -= startClosingLength;
                        return true;
                    }
                    break;
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
            } while (start >= startClosingLength);
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
            if (IsSeq(data, _gt, ref end)) return true;
            if (IsSeq(data, _space, ref end) || IsOtherSpace(data, ref end))
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

        if (IsSeq(data, _gt, ref end))
            return TagEnding.Closing;

        if (IsSeq(data, _slash, ref end))
            return IsSeq(data, _gt, ref end) ? TagEnding.SelfClosing : TagEnding.None;

        if (IsSeq(data, _space, ref end) || IsOtherSpace(data, ref end))
            return TagEnding.Name;

        return TagEnding.None;
    }

    protected override TagEnding GetEndingAttributeStart(ReadOnlySpan<T> data, ref int end)
    {
        Debug.Assert(end >= 0);

        while (end < data.Length)
        {
            if (IsSeq(data, _gt, ref end))
            {
                return TagEnding.Closing;
            }
            else if (IsSeq(data, _slash, ref end))
            {
                return IsSeq(data, _gt, ref end) ? TagEnding.SelfClosing : TagEnding.None;
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

    protected override TagEnding GetEndingHasAttributes(ReadOnlySpan<T> data, ref int end)
    {
        Debug.Assert(end < data.Length);
        Debug.Assert(end >= 0);

        //TODO: _gt, _slash и space был проверен ранее
        //стоит пропустить этот символ?
        Debug.Assert(!IsSeq(data, _gt, end));
        Debug.Assert(!IsSeq(data, _slash, end));

        do
        {
            if (IsSeq(data, _gt, ref end))
            {
                return TagEnding.ClosingHasAttributes;
            }
            else if (IsSeq(data, _slash, ref end))
            {
                return IsSeq(data, _gt, ref end) ? TagEnding.SelfClosingHasAttributes : TagEnding.None;
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
        } while (end < data.Length);

        return TagEnding.None;
    }

    #region Private

    private bool IsOtherSpace(ReadOnlySpan<T> span, ref int end)
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

    private static bool IsSeq(ReadOnlySpan<T> data, ReadOnlySpan<T> value, ref int start)
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

    private static bool IsSeq(ReadOnlySpan<T> data, ReadOnlySpan<T> value, int start)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(value.Length > 0);

        return start + value.Length <= data.Length && data.Slice(start, value.Length).SequenceEqual(value);
    }

    #endregion
}