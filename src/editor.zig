const std = @import("std");

const ast = @import("ast/lib.zig");
const common = @import("common.zig");
const symbol = @import("symbol/lib.zig");

pub const DataEditor = struct {
    ast: *ast.AST,
    symbol_table: *symbol.SymbolTable,

    const Self = @This();

    pub fn init(tree: *ast.AST, symbol_table: *symbol.SymbolTable) Self {
        return .{
            .ast = tree,
            .symbol_table = symbol_table,
        };
    }

    pub fn nextSpan(self: *Self) common.SourceSpan {
        return self.ast.nextSpan();
    }

    pub fn new(
        self: *Self,
        span: common.SourceSpan,
        data: ast.StatementData,
    ) !ast.StatementIndex {
        return try self.ast.append(ast.Statement.init(
            span,
            data,
        ));
    }

    pub fn nodeStart(
        self: *Self,
        head: *?ast.StatementIndex,
        span: common.SourceSpan,
        value: ast.StatementIndex,
    ) !ast.StatementIndex {
        const next = head.*;

        const next_node = try self.new(
            span,
            .{ .node = .{
                .value = value,
                .previous = null,
                .next = next,
            } },
        );

        if (head.*) |hi| {
            const hs = self.ast.getPtrMut(hi) orelse return error.NotFound;

            switch (hs.data) {
                .node => |*n| {
                    n.previous = next_node;
                },
                else => {},
            }
        }

        head.* = next_node;

        return next_node;
    }

    pub fn nodeEnd(
        self: *Self,
        head: *?ast.StatementIndex,
        tail: *?ast.StatementIndex,
        span: common.SourceSpan,
        value: ast.StatementIndex,
    ) !ast.StatementIndex {
        const next_node = try self.new(
            span,
            .{ .node = .{
                .value = value,
                .previous = tail.*,
                .next = null,
            } },
        );

        if (tail.*) |ti| {
            const ts = self.ast.getPtrMut(ti) orelse return error.NotFound;

            switch (ts.data) {
                .node => |*n| {
                    n.next = next_node;
                },
                else => {},
            }
        } else {
            head.* = next_node;
        }

        tail.* = next_node;

        return next_node;
    }

    pub fn scopeIdentifier(self: *Self, scope: ast.Scope, thread: bool, span: common.SourceSpan, sidx: usize) !ast.StatementIndex {
        const idx = try self.new(
            span,
            .{ .identifier = .{
                .scope = scope,
                .thread = thread,
            } },
        );

        const instance: symbol.Instance = .{ .span = span };
        _ = try self.symbol_table.addSymbolInstanceByIndex(sidx, &instance);

        return idx;
    }

    pub fn localIdentifier(self: *Self, sidx: usize, span: common.SourceSpan) !ast.StatementIndex {
        return self.scopeIdentifier(.local, false, span, sidx);
    }

    pub fn globalIdentifier(self: *Self, sidx: usize, span: common.SourceSpan) !ast.StatementIndex {
        return self.scopeIdentifier(.global, false, span, sidx);
    }

    pub fn typeIdentifier(self: *Self, sidx: usize, span: common.SourceSpan) !ast.StatementIndex {
        return self.scopeIdentifier(.type, false, span, sidx);
    }

    pub fn labelIdentifier(self: *Self, sidx: usize, span: common.SourceSpan) !ast.StatementIndex {
        return self.scopeIdentifier(.label, false, span, sidx);
    }

    pub fn symbolType(self: *Self, sidx: usize, span: common.SourceSpan) !ast.StatementIndex {
        const bsym: *const symbol.Symbol = self.symbol_table.getSymbolPtr(sidx) orelse return error.NotFound;
        const sym: *const symbol.Symbol = switch (bsym.memory) {
            .child => |v| self.symbol_table.getSymbolPtr(v.parent) orelse return error.NotFound,
            else => bsym,
        };

        return switch (sym.memory) {
            .primitive => |v| self.new(
                span,
                .{ .primitive_type = v },
            ),
            .type => |v| try self.typeIdentifier(v, span),
            else => error.NotFound,
        };
    }

    pub fn phiLine(self: *Self, block: usize, sidx: usize) !usize {
        const stat: ast.Statement = self.ast.get(block) orelse return error.NotFound;

        switch (stat.data) {
            .block => |b| {
                var out = b;

                const identifier = try self.localIdentifier(sidx, self.nextSpan());

                const phi = try self.new(
                    .{},
                    .{
                        .phi = .{
                            .data_type = try self.symbolType(sidx, self.nextSpan()),
                            .parameters = null,
                        },
                    },
                );

                const assignment = try self.new(
                    .{},
                    .{
                        .assignment = .{
                            .identifier = identifier,
                            .statement = phi,
                        },
                    },
                );

                const line = try self.new(.{}, .{
                    .line = assignment,
                });

                _ = try self.nodeStart(&out.phis, .{}, line);

                var statPtr = self.ast.getPtrMut(block) orelse return error.NotFound;
                statPtr.data = .{ .block = out };

                return phi;
            },
            else => unreachable,
        }
    }

    pub fn phiParameter(self: *Self, phi: usize, lidx: usize, sidx: usize) !usize {
        const stat: ast.Statement = self.ast.get(phi) orelse return error.NotFound;

        switch (stat.data) {
            .phi => |p| {
                var out = p;

                const identifier = try self.labelIdentifier(lidx, self.nextSpan());
                const value = try self.localIdentifier(sidx, self.nextSpan());

                const parameter = try self.new(
                    .{},
                    .{
                        .phi_parameter = .{
                            .identifier = identifier,
                            .value = value,
                        },
                    },
                );

                _ = try self.nodeStart(&out.parameters, .{}, parameter);

                const statPtr: *ast.Statement = self.ast.getPtrMut(phi) orelse return error.NotFound;
                statPtr.data = .{ .phi = out };

                return parameter;
            },
            else => unreachable,
        }
    }

    pub fn childSymbol(self: *Self, sidx: usize, index: usize) !usize {
        const allocator = self.symbol_table.allocator;

        const sym = self.symbol_table.getSymbolPtr(sidx) orelse return error.NotFound;

        var id = sym.identifier;
        id.name = try std.fmt.allocPrint(allocator, "{s}_{}", .{ id.name, index });
        defer allocator.free(id.name);

        const nsidx = try self.symbol_table.addSymbol(&id);
        var nsym = self.symbol_table.getSymbolPtrMut(nsidx).?;

        nsym.memory = .{ .child = .{ .parent = sidx, .index = index } };

        return nsidx;
    }
};
