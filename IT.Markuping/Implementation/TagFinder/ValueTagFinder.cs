using IT.Markuping.Extensions;
using IT.Markuping.Interfaces;
using System;
using System.Diagnostics;

namespace IT.Markuping.Implementation;

public ref struct ValueTagFinder<T>
{
    private ReadOnlySpan<T> _data;
    private ReadOnlySpan<T> _name;
    private ITagStrategy<T> _strategy;

    #region Props

    public ReadOnlySpan<T> Data
    {
        readonly get => _data;
        set => _data = value;
    }

    public ReadOnlySpan<T> Name
    {
        readonly get => _name;
        set
        {
            Debug.Assert(value.Length > 0);
            _name = value;
        }
    }

    public ITagStrategy<T> Strategy
    {
        readonly get => _strategy;
        set => _strategy = value ?? throw new ArgumentNullException(nameof(value));
    }

    #endregion Props

    #region Public Methods

    public ValueTagFinder(ReadOnlySpan<T> data, ReadOnlySpan<T> name, ITagStrategy<T> strategy)
    {
        if (strategy == null) throw new ArgumentNullException(nameof(strategy));
        Debug.Assert(name.Length > 0);

        _data = data;
        _name = name;
        _strategy = strategy;
    }

    public Tag First(out TagNS ns, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var data = _data;
        var len = data.Length;
        var min = _strategy.LtLength;
        do
        {
            var index = _strategy.IndexOf(data, _name);
            if (index < 0) break;

            var end = index + namelen;
            if (index >= min)
            {
                var tag = GetTag(data, index, end, endings, out ns);
                if (!tag.IsEmpty)
                {
                    var offset = len - data.Length;
                    if (!ns.IsEmpty) ns = ns.AddOffset(offset);
                    return tag.AddOffset(offset);
                }
            }

            data = data.Slice(end);
        } while (true);

        ns = default;
        return default;
    }

    public Tag First(ReadOnlySpan<T> ns, TagEndings endings = default)
        => ns.IsEmpty ? First(endings) : FirstNS(ns, endings);

    public Tag First(TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var data = _data;
        var len = data.Length;
        var min = _strategy.LtLength;
        do
        {
            var index = _strategy.IndexOf(data, _name);
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

    public Tag Last(out TagNS ns, TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        int index = _data.Length;
        var min = _strategy.LtLength;
        do
        {
            index = _strategy.LastIndexOf(_data.Slice(0, index), _name);
            if (index < min) break;

            var tag = GetTag(_data, index, index + namelen, endings, out ns);
            if (!tag.IsEmpty)
            {
                return tag;
            }
        } while (true);

        ns = default;
        return default;
    }

    public Tag Last(ReadOnlySpan<T> ns, TagEndings endings = default)
        => ns.IsEmpty ? Last(endings) : LastNS(ns, endings);

    public Tag Last(TagEndings endings = default)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var min = _strategy.LtLength;
        int index = _data.Length;
        do
        {
            index = _strategy.LastIndexOf(_data.Slice(0, index), _name);
            if (index < min) break;

            var tag = GetTag(_data, index - min, index + namelen, endings);
            if (!tag.IsEmpty)
            {
                return tag;
            }
        } while (true);

        return default;
    }

    public TagClosing FirstClosing(ReadOnlySpan<T> ns)
        => ns.IsEmpty ? FirstClosing() : FirstClosingNS(ns);

    public TagClosing FirstClosing()
    {
        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var data = _data;
        var len = data.Length;
        var min = _strategy.LtSlashLength;
        do
        {
            var index = _strategy.IndexOf(data, _name);
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

    public TagClosing LastClosing(out TagNS ns)
    {
        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var data = _data;
        var min = _strategy.LtSlashLength;
        do
        {
            var index = _strategy.LastIndexOf(data, _name);
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

    public TagClosing LastClosing(ReadOnlySpan<T> ns)
        => ns.IsEmpty ? LastClosing() : LastClosingNS(ns);

    public TagClosing LastClosing()
    {
        var namelen = _name.Length;
        Debug.Assert(namelen > 0);

        var data = _data;
        var min = _strategy.LtSlashLength;
        do
        {
            var index = _strategy.LastIndexOf(data, _name);
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

    #endregion Public Methods

    #region Private Methods

    private Tag FirstNS(ReadOnlySpan<T> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var data = _data;
        var len = data.Length;
        //"<ns:"
        var min = nslen + _strategy.LtColonLength;
        do
        {
            var index = _strategy.IndexOf(data, _name);
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

    private Tag LastNS(ReadOnlySpan<T> ns, TagEndings endings)
    {
        if (!endings.IsValid()) throw new ArgumentOutOfRangeException(nameof(endings));

        var namelen = _name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var min = nslen + _strategy.LtColonLength;
        int index = _data.Length;
        do
        {
            index = _strategy.LastIndexOf(_data.Slice(0, index), _name);
            if (index < min) break;

            var tag = GetTag(_data, index - min, index + namelen, endings, ns);
            if (!tag.IsEmpty)
            {
                return tag;
            }
        } while (true);

        return default;
    }

    private TagClosing FirstClosingNS(ReadOnlySpan<T> ns)
    {
        var namelen = _name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var data = _data;
        var len = _data.Length;
        var min = nslen + _strategy.LtSlashColonLength;//</ns:
        do
        {
            var index = _strategy.IndexOf(data, _name);
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

    private TagClosing LastClosingNS(ReadOnlySpan<T> ns)
    {
        var namelen = _name.Length;
        var nslen = ns.Length;

        Debug.Assert(namelen > 0);
        Debug.Assert(nslen > 0);

        var data = _data;
        var min = nslen + _strategy.LtSlashColonLength;//</ns:
        do
        {
            var index = _strategy.LastIndexOf(_data, _name);
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

        if (end < data.Length && _strategy.IsStartOpening(data, start))
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

        if (end < data.Length && _strategy.IsStartOpening(data, start, ns))
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

    private Tag GetTag(ReadOnlySpan<T> data, int start, int end, TagEndings endings, out TagNS ns)
    {
        Debug.Assert(end > 0 && start < end);

        if (end < data.Length && _strategy.IsStartOpening(data, ref start, out ns))
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

    private TagClosing GetClosing(ReadOnlySpan<T> data, int start, int end, out TagNS ns)
    {
        Debug.Assert(start > 0 && end > 0);
        Debug.Assert(start < end);

        if (end < data.Length && _strategy.IsStartClosing(data, ref start, out ns) &&
            _strategy.IsEndClosing(data, ref end, out var hasSpace))
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

        if (end < data.Length && _strategy.IsStartClosing(data, start) &&
            _strategy.IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    private TagClosing GetClosing(ReadOnlySpan<T> data, int start, int end, ReadOnlySpan<T> ns)
    {
        Debug.Assert(start >= 0 && end > 0);
        Debug.Assert(start < end);

        if (end < data.Length && _strategy.IsStartClosing(data, start, ns) &&
            _strategy.IsEndClosing(data, ref end, out var hasSpace))
        {
            return new(start, end, hasSpace: hasSpace);
        }
        return default;
    }

    private TagEnding GetEndingAnyClosing(ReadOnlySpan<T> data, ref int end)
    {
        var ending = _strategy.GetEndingName(data, ref end);
        if (ending == TagEnding.Name)
        {
            ending = _strategy.GetEndingAttributeStart(data, ref end);
            if (ending == TagEnding.AttributeStart)
                ending = _strategy.GetEndingHasAttributes(data, ref end);
        }

        return ending;
    }

    private TagEnding GetEnding(ReadOnlySpan<T> data, ref int end, TagEndings endings)
    {
        var ending = _strategy.GetEndingName(data, ref end);
        if (endings == TagEndings.HasNoAttributes)
            return ending;

        var hasAttributes = endings.HasAttributes();
        var hasNoAttributes = endings.HasNoAttributes();

        if (ending == TagEnding.Name)
        {
            ending = _strategy.GetEndingAttributeStart(data, ref end);
            if (endings == TagEndings.HasAttributes)
                return ending == TagEnding.AttributeStart ? TagEnding.AttributeStart : TagEnding.None;

            if (ending == TagEnding.AttributeStart && (hasAttributes || !hasNoAttributes))
            {
                ending = _strategy.GetEndingHasAttributes(data, ref end);
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