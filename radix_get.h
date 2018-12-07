typedef struct
{
    U32 length;
    U32 dist;
} RMF_match;

static size_t RMF_bitpackExtendMatch(const BYTE* const data,
    const U32* const table,
    ptrdiff_t const start_index,
    ptrdiff_t limit,
    U32 const link,
    size_t const length)
{
    ptrdiff_t end_index = start_index + length;
    ptrdiff_t dist = start_index - link;
    if (limit > start_index + (ptrdiff_t)kMatchLenMax)
        limit = start_index + kMatchLenMax;
    while (end_index < limit && end_index - (ptrdiff_t)(table[end_index] & RADIX_LINK_MASK) == dist) {
        end_index += table[end_index] >> RADIX_LINK_BITS;
    }
    if (end_index >= limit) {
        DEBUGLOG(7, "RMF_bitpackExtendMatch : pos %u, link %u, init length %u, full length %u", (U32)start_index, link, (U32)length, (U32)(limit - start_index));
        return limit - start_index;
    }
    while (end_index < limit && data[end_index - dist] == data[end_index]) {
        ++end_index;
    }
    DEBUGLOG(7, "RMF_bitpackExtendMatch : pos %u, link %u, init length %u, full length %u", (U32)start_index, link, (U32)length, (U32)(end_index - start_index));
    return end_index - start_index;
}

#define GetMatchLink(table, index) ((const RMF_unit*)(table))[(index) >> UNIT_BITS].links[(index) & UNIT_MASK]

#define GetMatchLength(table, index) ((const RMF_unit*)(table))[(index) >> UNIT_BITS].lengths[(index) & UNIT_MASK]

static size_t RMF_structuredExtendMatch(const BYTE* const data,
    const U32* const table,
    ptrdiff_t const start_index,
    ptrdiff_t limit,
    U32 const link,
    size_t const length)
{
    ptrdiff_t end_index = start_index + length;
    ptrdiff_t dist = start_index - link;
    if (limit > start_index + (ptrdiff_t)kMatchLenMax)
        limit = start_index + kMatchLenMax;
    while (end_index < limit && end_index - (ptrdiff_t)GetMatchLink(table, end_index) == dist) {
        end_index += GetMatchLength(table, end_index);
    }
    if (end_index >= limit) {
        DEBUGLOG(7, "RMF_structuredExtendMatch : pos %u, link %u, init length %u, full length %u", (U32)start_index, link, (U32)length, (U32)(limit - start_index));
        return limit - start_index;
    }
    while (end_index < limit && data[end_index - dist] == data[end_index]) {
        ++end_index;
    }
    DEBUGLOG(7, "RMF_structuredExtendMatch : pos %u, link %u, init length %u, full length %u", (U32)start_index, link, (U32)length, (U32)(end_index - start_index));
    return end_index - start_index;
}

FORCE_INLINE_TEMPLATE
RMF_match RMF_getMatch(FL2_dataBlock block,
    FL2_matchTable* tbl,
    unsigned max_depth,
    int structTbl,
    size_t index)
{
    if (structTbl)
    {
        RMF_match match;
        U32 link = GetMatchLink(tbl->table, index);
        size_t length;
        size_t dist;
        match.length = 0;
        if (link == RADIX_NULL_LINK)
            return match;
        length = GetMatchLength(tbl->table, index);
        dist = index - link - 1;
        if (length > block.end - index) {
            match.length = (U32)(block.end - index);
        }
        else if (length == max_depth
            || length == STRUCTURED_MAX_LENGTH /* from HandleRepeat */)
        {
            match.length = (U32)RMF_structuredExtendMatch(block.data, tbl->table, index, block.end, link, length);
        }
        else {
            match.length = (U32)length;
        }
        match.dist = (U32)dist;
        return match;
    }
    else {
        RMF_match match;
        U32 link = tbl->table[index];
        size_t length;
        size_t dist;
        match.length = 0;
        if (link == RADIX_NULL_LINK)
            return match;
        length = link >> RADIX_LINK_BITS;
        link &= RADIX_LINK_MASK;
        dist = index - link - 1;
        if (length > block.end - index) {
            match.length = (U32)(block.end - index);
        }
        else if (length == max_depth
            || length == BITPACK_MAX_LENGTH /* from HandleRepeat */)
        {
            match.length = (U32)RMF_bitpackExtendMatch(block.data, tbl->table, index, block.end, link, length);
        }
        else {
            match.length = (U32)length;
        }
        match.dist = (U32)dist;
        return match;
    }
}

FORCE_INLINE_TEMPLATE
RMF_match RMF_getNextMatch(FL2_dataBlock block,
    FL2_matchTable* tbl,
    unsigned max_depth,
    int structTbl,
    size_t index)
{
    if (structTbl)
    {
        RMF_match match;
        U32 link = GetMatchLink(tbl->table, index);
        size_t length;
        size_t dist;
        match.length = 0;
        if (link == RADIX_NULL_LINK)
            return match;
        length = GetMatchLength(tbl->table, index);
        dist = index - link - 1;
        if (link - 1 == GetMatchLink(tbl->table, index - 1)) {
            /* same as the previous match, one byte shorter */
            return match;
        }
        if (length > block.end - index) {
            match.length = (U32)(block.end - index);
        }
        else if (length == max_depth
            || length == STRUCTURED_MAX_LENGTH /* from HandleRepeat */)
        {
            match.length = (U32)RMF_structuredExtendMatch(block.data, tbl->table, index, block.end, link, length);
        }
        else {
            match.length = (U32)length;
        }
        match.dist = (U32)dist;
        return match;
    }
    else {
        RMF_match match;
        U32 link = tbl->table[index];
        size_t length;
        size_t dist;
        match.length = 0;
        if (link == RADIX_NULL_LINK)
            return match;
        length = link >> RADIX_LINK_BITS;
        link &= RADIX_LINK_MASK;
        dist = index - link - 1;
        if (link - 1 == (tbl->table[index - 1] & RADIX_LINK_MASK)) {
            /* same distance, one byte shorter */
            return match;
        }
        if (length > block.end - index) {
            match.length = (U32)(block.end - index);
        }
        else if (length == max_depth
            || length == BITPACK_MAX_LENGTH /* from HandleRepeat */)
        {
            match.length = (U32)RMF_bitpackExtendMatch(block.data, tbl->table, index, block.end, link, length);
        }
        else {
            match.length = (U32)length;
        }
        match.dist = (U32)dist;
        return match;
    }
}

