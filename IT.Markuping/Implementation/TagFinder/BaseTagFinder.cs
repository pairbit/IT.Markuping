using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

public abstract class BaseTagFinder<T> : ITagFinder<T> where T : unmanaged
{
    protected abstract int LtLength { get; }

    protected abstract int LtColonLength { get; }

    protected abstract int LtSlashLength { get; }

    protected abstract int LtSlashColonLength { get; }

    #region Protected Methods

    //protected abstract string GetString(ReadOnlySpan<T> data);

    protected abstract int IndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value);

    protected abstract int LastIndexOf(ReadOnlySpan<T> data, ReadOnlySpan<T> value);

    protected abstract bool IsStartOpening(ReadOnlySpan<T> data, int start);

    protected abstract bool IsStartOpening(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns);

    protected abstract bool IsStartOpening(ReadOnlySpan<T> data, ref int start, out Range ns);

    protected abstract bool IsStartClosing(ReadOnlySpan<T> data, int start);

    protected abstract bool IsStartClosing(ReadOnlySpan<T> data, int start, ReadOnlySpan<T> ns);

    protected abstract bool IsStartClosing(ReadOnlySpan<T> data, ref int start, out Range ns);

    protected abstract bool IsEndClosing(ReadOnlySpan<T> data, ref int end, out bool hasSpace);

    protected abstract TagEnding GetEndingName(ReadOnlySpan<T> data, ref int end);

    protected abstract TagEnding GetEndingAttributeStart(ReadOnlySpan<T> data, ref int end);

    protected abstract TagEnding GetEndingHasAttributes(ReadOnlySpan<T> data, ref int end);

    #endregion Protected Methods

    #region ITagFinder

    public Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, out Range ns)
    {
        var opening = First(data, name, out ns, TagEndings.Closing);
        if (!opening.IsEmpty)
        {
            var closing = FirstClosing(data, name, data[ns], opening.End, out nodes);
            if (!closing.IsEmpty)
            {
                return new(opening, closing);
            }
        }
        nodes = default;
        return default;
    }

    public Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, ReadOnlySpan<T> ns)
    {
        var opening = First(data, name, ns, TagEndings.Closing);
        if (!opening.IsEmpty)
        {
            var closing = FirstClosing(data, name, ns, opening.End, out nodes);
            if (!closing.IsEmpty)
            {
                return new(opening, closing);
            }
        }
        nodes = default;
        return default;
    }

    public Tags FirstPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes)
    {
        var opening = First(data, name, TagEndings.Closing);
        if (!opening.IsEmpty)
        {
            var closing = FirstClosing(data, name, default, opening.End, out nodes);
            if (!closing.IsEmpty)
            {
                return new(opening, closing);
            }
        }
        nodes = default;
        return default;
    }

    public Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, out Range ns)
    {
        var closing = LastClosing(data, name, out ns);
        if (!closing.IsEmpty)
        {
            var opening = LastOpening(data.Slice(0, closing.Start), name, data[ns], out nodes);
            if (!opening.IsEmpty)
            {
                return new(opening, closing, nodes > 0);
            }
        }
        nodes = default;
        ns = default;
        return default;
    }

    public Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes, ReadOnlySpan<T> ns)
    {
        var closing = LastClosing(data, name, ns);
        if (!closing.IsEmpty)
        {
            var opening = LastOpening(data.Slice(0, closing.Start), name, ns, out nodes);
            if (!opening.IsEmpty)
            {
                return new(opening, closing, nodes > 0);
            }
        }
        nodes = default;
        return default;
    }

    public Tags LastPair(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out int nodes)
    {
        var closing = LastClosing(data, name);
        if (!closing.IsEmpty)
        {
            var opening = LastOpening(data.Slice(0, closing.Start), name, default, out nodes);
            if (!opening.IsEmpty)
            {
                return new(opening, closing, nodes > 0);
            }
        }
        nodes = default;
        return default;
    }

    public Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var len = data.Length;
        var min = LtLength;
        do
        {
            var index = IndexOf(data, name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var tag = GetTag(data, index, end, endings, out ns);
                if (!tag.IsEmpty)
                {
                    var offset = len - data.Length;
                    ns = ns.AddOffsetIfNotZero(offset);
                    return tag.AddOffset(offset);
                }
            }

            data = data.Slice(end);
        } while (true);

        ns = default;
        return default;
    }

    public Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default)
        => ns.IsEmpty ? First(data, name, endings) : FirstNS(data, name, ns, endings);

    public Tag First(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var len = data.Length;
        var min = LtLength;
        do
        {
            var index = IndexOf(data, name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var tag = GetTag(data, index - min, end, endings);
                if (!tag.IsEmpty)
                {
                    return tag.AddOffset(len - data.Length);
                }
            }

            data = data.Slice(end);
        } while (true);

        return default;
    }

    public Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings = default)
        => ns.IsEmpty ? Last(data, name, endings) : LastNS(data, name, ns, endings);

    public Tag Last(ReadOnlySpan<T> data, ReadOnlySpan<T> name, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var min = LtLength;
        int index = data.Length;
        do
        {
            index = LastIndexOf(data.Slice(0, index), name);
            if (index < min) break;

            var tag = GetTag(data, index - min, index + namelen, endings);
            if (!tag.IsEmpty)
            {
                return tag;
            }
        } while (true);

        return default;
    }

    public TagClosing FirstClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
        => ns.IsEmpty ? FirstClosing(data, name) : FirstClosingNS(data, name, ns);

    public TagClosing FirstClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var len = data.Length;
        var min = LtSlashLength;
        do
        {
            var index = IndexOf(data, name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var closing = GetClosing(data, index - min, end);
                if (!closing.IsEmpty)
                {
                    return closing.AddOffset(len - data.Length);
                }
            }
            data = data.Slice(end);
        } while (true);

        return default;
    }

    public TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, out Range ns)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var min = LtSlashLength;
        do
        {
            var index = LastIndexOf(data, name);
            if (index < min) break;

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

    public TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
        => ns.IsEmpty ? LastClosing(data, name) : LastClosingNS(data, name, ns);

    public TagClosing LastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name)
    {
        var namelen = name.Length;
        Debug.Assert(namelen > 0);

        var min = LtSlashLength;
        do
        {
            var index = LastIndexOf(data, name);
            if (index < min) break;

            var closing = GetClosing(data, index - min, index + namelen);
            if (!closing.IsEmpty)
            {
                return closing;
            }

            data = data.Slice(0, index);
        } while (true);

        return default;
    }

    #endregion

    #region Private Methods

    private TagClosing FirstClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, int offset, out int nodes)
    {
        Debug.Assert(data.Length >= offset);

        nodes = 0;
        int count = 1;
        data = data.Slice(offset);
        do
        {
#if DEBUG
            //var str = GetString(data);
#endif

            var closing = FirstClosing(data, name, ns);
            if (closing.IsEmpty) break;

            count += CountFirst(data.Slice(0, closing.Start), name, ns) - 1;
            if (count == 0)
                return closing.AddOffset(offset, nodes > 0);

            nodes++;
            data = data.Slice(closing.End);
            offset += closing.End;
        } while (true);

        return default;
    }

    private int CountFirst(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
    {
        int count = 0;
        do
        {
#if DEBUG
            //var str = GetString(data);
#endif

            var opening = First(data, name, ns, TagEndings.Closing);
            if (opening.IsEmpty) break;

            data = data.Slice(opening.End);
            count++;
        } while (true);
        return count;
    }

    private Tag LastOpening(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, out int nodes)
    {
        int count = 1;
        nodes = 0;
        do
        {
#if DEBUG
            //var str = GetString(data);
#endif

            var opening = Last(data, name, ns, TagEndings.Closing);
            if (opening.IsEmpty) break;

            count += CountLastClosing(data.Slice(opening.End), name, ns) - 1;
            if (count == 0)
                return opening;

            nodes++;
            data = data.Slice(0, opening.Start);
        } while (true);

        return default;
    }

    private int CountLastClosing(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
    {
        int count = 0;
        do
        {
#if DEBUG
            //var str = GetString(data);
#endif

            var closing = LastClosing(data, name, ns);
            if (closing.IsEmpty) break;

            data = data.Slice(0, closing.Start);
            count++;
        } while (true);
        return count;
    }

    private Tag FirstNS(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var len = data.Length;
        //"<ns:"
        var min = nslen + LtColonLength;
        do
        {
            var index = IndexOf(data, name);
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

    private Tag LastNS(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + LtColonLength;
        int index = data.Length;
        do
        {
            index = LastIndexOf(data.Slice(0, index), name);
            if (index < min) break;

            var tag = GetTag(data, index - min, index + namelen, endings, ns);
            if (!tag.IsEmpty)
            {
                return tag;
            }
        } while (true);

        return default;
    }

    private TagClosing FirstClosingNS(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
    {
        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var len = data.Length;
        var min = nslen + LtSlashColonLength;//</ns:
        do
        {
            var index = IndexOf(data, name);
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

    private TagClosing LastClosingNS(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ReadOnlySpan<T> ns)
    {
        var namelen = name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + LtSlashColonLength;//</ns:
        do
        {
            var index = LastIndexOf(data, name);
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

    private Tag GetTag(ReadOnlySpan<T> data, int start, int end, TagEndings endings)
    {
        Debug.Assert(start >= 0);
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && IsStartOpening(data, start))
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

    private Tag GetTag(ReadOnlySpan<T> data, int start, int end, TagEndings endings, ReadOnlySpan<T> ns)
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

    private Tag GetTag(ReadOnlySpan<T> data, int start, int end, TagEndings endings, out Range ns)
    {
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && IsStartOpening(data, ref start, out ns))
        {
            var ending = endings.IsAnyClosing() ?
                GetEndingAnyClosing(data, ref end) :
                GetEnding(data, ref end, endings);
            if (ending != TagEnding.None)
            {
                return new(start, end, ending);
            }
        }
        ns = default;
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<T> data, int start, int end, out Range ns)
    {
        Debug.Assert(start > 0 && end > 0);
        Debug.Assert(start < end);

        if (end < data.Length && IsStartClosing(data, ref start, out ns) &&
            IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        ns = default;
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<T> data, int start, int end)
    {
        Debug.Assert(start >= 0 && start + 1 < data.Length);
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && IsStartClosing(data, start) &&
            IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<T> data, int start, int end, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0 && end > 0);
        Debug.Assert(start < end);

        if (end < data.Length && IsStartClosing(data, start, ns) &&
            IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    private TagEnding GetEndingAnyClosing(ReadOnlySpan<T> data, ref int end)
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

    private TagEnding GetEnding(ReadOnlySpan<T> data, ref int end, TagEndings endings)
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

    #endregion Private Methods
}