using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

internal class StrictTagFinder<T> : BaseTagFinder<T> where T : unmanaged, IEquatable<T>
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

        public Tokens(T lt, T gt, T slash, T colon, T space, T quot, T eq)
        {
            _lt = lt;
            _gt = gt;
            _slash = slash;
            _colon = colon;
            _space = space;
            _quot = quot;
            _eq = eq;
        }
    }

    protected readonly Tokens _tokens;

    protected override int LtLength => 1;

    protected override int LtColonLength => 2;

    protected override int LtSlashLength => 2;

    protected override int LtSlashColonLength => 3;

    public StrictTagFinder(Tokens tokens)
    {
        _tokens = tokens;
    }

    protected virtual bool IsSpace(T value) => value.Equals(_tokens._space);

    protected override int IndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value)
        => data.IndexOf(value);

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
               data.Slice(start, ns.Length).SequenceEqual(ns);
    }

    protected override bool IsStartOpening(ReadOnlySpan<T> data, ref int start, out Range ns)
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
                    Debug.Assert(endNS > start + 1);

                    ns = (start + 1)..endNS;
                    return true;
                }
                else if (token.Equals(_tokens._quot))
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
               data.Slice(start, ns.Length).SequenceEqual(ns);
    }

    protected override bool IsStartClosing(ReadOnlySpan<T> data, ref int start, out Range ns)
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
                        Debug.Assert(endNS > start + 2);

                        ns = (start + 2)..endNS;
                        return true;
                    }
                    break;
                }
                else if (token.Equals(_tokens._quot))
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
        Debug.Assert(!data[end].Equals(_tokens._gt));
        Debug.Assert(!data[end].Equals(_tokens._slash));

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
        } while (end < data.Length);

        return TagEnding.None;
    }
}