#!/usr/bin/env python3
"""
Graphiti Explorer - 交互式知识图谱浏览器

使用方法:
    python graphiti_explorer.py [--port 8092] [--host localhost]

功能:
    - 实时查看知识图谱内容
    - 搜索和过滤功能
    - 可视化关系图
    - 导出功能
"""

import json
import requests
import argparse
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
import time
import os
import sys

# 尝试导入可选依赖
try:
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
    from rich.layout import Layout
    from rich.live import Live
    from rich.progress import Progress, SpinnerColumn, TextColumn
    RICH_AVAILABLE = True
except ImportError:
    RICH_AVAILABLE = False
    print("提示: 安装 'rich' 库以获得更好的显示效果")
    print("pip install rich")

class GraphitiClient:
    """Graphiti MCP 客户端"""
    
    def __init__(self, host: str = "localhost", port: int = 8092):
        self.base_url = f"http://{host}:{port}/mcp"
        self.session = requests.Session()
        
    def call_tool(self, tool_name: str, arguments: Dict[str, Any]) -> Optional[Dict]:
        """调用MCP工具"""
        payload = {
            "jsonrpc": "2.0",
            "id": int(time.time() * 1000),
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": arguments
            }
        }
        
        try:
            response = self.session.post(self.base_url, json=payload, timeout=5)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            print(f"错误: {e}")
            return None
    
    def search_memory(self, query: str = "*", limit: int = 10) -> List[Dict]:
        """搜索记忆"""
        result = self.call_tool("search_memory", {
            "query": query,
            "limit": limit
        })
        
        if result and "result" in result:
            # 解析返回的文本内容
            content = result["result"].get("content", [])
            if content and len(content) > 0:
                text = content[0].get("text", "")
                return self._parse_search_results(text)
        return []
    
    def search_code(self, query: str = "*", entity_type: Optional[str] = None, 
                   limit: int = 10) -> List[Dict]:
        """搜索代码实体"""
        args = {
            "query": query,
            "limit": limit
        }
        if entity_type:
            args["entity_type"] = entity_type
            
        result = self.call_tool("search_code", args)
        
        if result and "result" in result:
            content = result["result"].get("content", [])
            if content and len(content) > 0:
                text = content[0].get("text", "")
                return self._parse_code_results(text)
        return []
    
    def add_memory(self, content: str, source: str = "手动输入") -> bool:
        """添加记忆"""
        result = self.call_tool("add_memory", {
            "content": content,
            "source": source
        })
        return result is not None and "error" not in result
    
    def get_stats(self) -> Dict[str, int]:
        """获取统计信息"""
        # 通过搜索获取总数
        memory_result = self.call_tool("search_memory", {"query": "*", "limit": 1})
        code_result = self.call_tool("search_code", {"query": "*", "limit": 1})
        
        stats = {
            "total_memories": 0,
            "total_entities": 0,
            "today_memories": 0,
            "today_entities": 0
        }
        
        # 解析总数
        if memory_result and "result" in memory_result:
            text = memory_result["result"].get("content", [{}])[0].get("text", "")
            if "Found" in text:
                try:
                    stats["total_memories"] = int(text.split("Found ")[1].split(" ")[0])
                except:
                    pass
                    
        if code_result and "result" in code_result:
            text = code_result["result"].get("content", [{}])[0].get("text", "")
            if "Found" in text:
                try:
                    stats["total_entities"] = int(text.split("Found ")[1].split(" ")[0])
                except:
                    pass
                    
        return stats
    
    def _parse_search_results(self, text: str) -> List[Dict]:
        """解析搜索结果文本"""
        results = []
        lines = text.strip().split('\n')
        
        for line in lines:
            if line.strip() and not line.startswith("Found"):
                results.append({
                    "content": line.strip(),
                    "timestamp": datetime.now().isoformat()
                })
        
        return results
    
    def _parse_code_results(self, text: str) -> List[Dict]:
        """解析代码搜索结果"""
        results = []
        lines = text.strip().split('\n')
        
        for line in lines:
            if line.strip() and not line.startswith("Found"):
                # 尝试解析格式: [Type] Name - Description
                parts = line.strip().split(' - ', 1)
                if len(parts) == 2:
                    type_name = parts[0].strip()
                    description = parts[1].strip()
                    
                    # 提取类型和名称
                    if '[' in type_name and ']' in type_name:
                        entity_type = type_name[type_name.find('[')+1:type_name.find(']')]
                        name = type_name[type_name.find(']')+1:].strip()
                    else:
                        entity_type = "Unknown"
                        name = type_name
                    
                    results.append({
                        "type": entity_type,
                        "name": name,
                        "description": description
                    })
                else:
                    results.append({
                        "type": "Unknown",
                        "name": line.strip(),
                        "description": ""
                    })
        
        return results

class GraphitiExplorer:
    """Graphiti 知识图谱浏览器"""
    
    def __init__(self, client: GraphitiClient):
        self.client = client
        self.console = Console() if RICH_AVAILABLE else None
        
    def run(self):
        """运行交互式浏览器"""
        while True:
            self.show_menu()
            choice = input("\n选择操作 (1-7): ").strip()
            
            if choice == '1':
                self.show_dashboard()
            elif choice == '2':
                self.search_memories()
            elif choice == '3':
                self.search_code_entities()
            elif choice == '4':
                self.add_new_memory()
            elif choice == '5':
                self.show_recent_activity()
            elif choice == '6':
                self.export_knowledge()
            elif choice == '7':
                print("👋 再见!")
                break
            else:
                print("无效选择，请重试")
    
    def show_menu(self):
        """显示主菜单"""
        if RICH_AVAILABLE and self.console:
            panel = Panel(
                "[1] 📊 查看仪表板\n"
                "[2] 🔍 搜索记忆\n"
                "[3] 🔧 搜索代码实体\n"
                "[4] ➕ 添加新记忆\n"
                "[5] 📈 查看最近活动\n"
                "[6] 💾 导出知识图谱\n"
                "[7] 🚪 退出",
                title="🧠 Graphiti Explorer",
                expand=False
            )
            self.console.print(panel)
        else:
            print("\n" + "="*50)
            print("🧠 Graphiti Explorer - 主菜单")
            print("="*50)
            print("[1] 📊 查看仪表板")
            print("[2] 🔍 搜索记忆")
            print("[3] 🔧 搜索代码实体")
            print("[4] ➕ 添加新记忆")
            print("[5] 📈 查看最近活动")
            print("[6] 💾 导出知识图谱")
            print("[7] 🚪 退出")
    
    def show_dashboard(self):
        """显示仪表板"""
        stats = self.client.get_stats()
        recent_memories = self.client.search_memory(limit=5)
        recent_entities = self.client.search_code(limit=5)
        
        if RICH_AVAILABLE and self.console:
            # 创建统计表格
            stats_table = Table(title="📊 知识图谱统计")
            stats_table.add_column("指标", style="cyan")
            stats_table.add_column("数量", style="green")
            
            stats_table.add_row("总记忆数", str(stats['total_memories']))
            stats_table.add_row("总实体数", str(stats['total_entities']))
            stats_table.add_row("今日新增记忆", str(stats['today_memories']))
            stats_table.add_row("今日新增实体", str(stats['today_entities']))
            
            self.console.print(stats_table)
            
            # 显示最近记忆
            if recent_memories:
                mem_table = Table(title="📝 最近记忆")
                mem_table.add_column("内容", style="yellow")
                for mem in recent_memories[:5]:
                    mem_table.add_row(mem['content'][:80] + "...")
                self.console.print(mem_table)
            
            # 显示最近实体
            if recent_entities:
                ent_table = Table(title="🔧 最近代码实体")
                ent_table.add_column("类型", style="blue")
                ent_table.add_column("名称", style="green")
                ent_table.add_column("描述", style="white")
                for ent in recent_entities[:5]:
                    ent_table.add_row(
                        ent.get('type', 'Unknown'),
                        ent.get('name', ''),
                        ent.get('description', '')[:40] + "..."
                    )
                self.console.print(ent_table)
        else:
            print("\n📊 知识图谱统计")
            print("-" * 50)
            print(f"总记忆数: {stats['total_memories']}")
            print(f"总实体数: {stats['total_entities']}")
            print(f"今日新增记忆: {stats['today_memories']}")
            print(f"今日新增实体: {stats['today_entities']}")
            
            print("\n📝 最近记忆:")
            for i, mem in enumerate(recent_memories[:5], 1):
                print(f"{i}. {mem['content'][:80]}...")
            
            print("\n🔧 最近代码实体:")
            for i, ent in enumerate(recent_entities[:5], 1):
                print(f"{i}. [{ent.get('type', 'Unknown')}] {ent.get('name', '')} - {ent.get('description', '')[:40]}...")
        
        input("\n按回车键继续...")
    
    def search_memories(self):
        """搜索记忆"""
        query = input("输入搜索关键词 (留空搜索全部): ").strip() or "*"
        results = self.client.search_memory(query, limit=20)
        
        if results:
            print(f"\n找到 {len(results)} 条记忆:")
            for i, mem in enumerate(results, 1):
                print(f"{i}. {mem['content']}")
        else:
            print("未找到相关记忆")
        
        input("\n按回车键继续...")
    
    def search_code_entities(self):
        """搜索代码实体"""
        query = input("输入搜索关键词 (留空搜索全部): ").strip() or "*"
        entity_type = input("实体类型过滤 (Class/Function/Module等，留空不过滤): ").strip() or None
        
        results = self.client.search_code(query, entity_type, limit=20)
        
        if results:
            print(f"\n找到 {len(results)} 个代码实体:")
            for i, ent in enumerate(results, 1):
                print(f"{i}. [{ent.get('type', 'Unknown')}] {ent.get('name', '')} - {ent.get('description', '')}")
        else:
            print("未找到相关代码实体")
        
        input("\n按回车键继续...")
    
    def add_new_memory(self):
        """添加新记忆"""
        print("\n➕ 添加新记忆")
        content = input("输入要记录的内容: ").strip()
        if not content:
            print("内容不能为空")
            return
        
        source = input("来源 (可选，默认'手动输入'): ").strip() or "手动输入"
        
        if self.client.add_memory(content, source):
            print("✅ 记忆添加成功!")
        else:
            print("❌ 添加失败")
        
        input("\n按回车键继续...")
    
    def show_recent_activity(self):
        """显示最近活动"""
        # 获取最近24小时的活动
        recent_memories = self.client.search_memory(limit=50)
        
        print("\n📈 最近24小时活动统计")
        print("-" * 50)
        
        # 按小时统计
        hour_stats = {}
        for mem in recent_memories:
            # 这里简化处理，实际应该解析时间戳
            hour = datetime.now().hour
            hour_stats[hour] = hour_stats.get(hour, 0) + 1
        
        # 显示活动图表
        max_count = max(hour_stats.values()) if hour_stats else 0
        for hour in range(24):
            count = hour_stats.get(hour, 0)
            bar = "█" * int((count / max_count * 20) if max_count > 0 else 0)
            print(f"{hour:02d}:00 {bar} {count}")
        
        input("\n按回车键继续...")
    
    def export_knowledge(self):
        """导出知识图谱"""
        print("\n💾 导出知识图谱")
        
        filename = input("输入导出文件名 (默认: graphiti_export.json): ").strip() or "graphiti_export.json"
        
        # 获取所有数据
        memories = self.client.search_memory(limit=1000)
        entities = self.client.search_code(limit=1000)
        
        export_data = {
            "export_time": datetime.now().isoformat(),
            "statistics": self.client.get_stats(),
            "memories": memories,
            "code_entities": entities
        }
        
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(export_data, f, ensure_ascii=False, indent=2)
            print(f"✅ 成功导出到 {filename}")
            print(f"   - 记忆数: {len(memories)}")
            print(f"   - 实体数: {len(entities)}")
        except Exception as e:
            print(f"❌ 导出失败: {e}")
        
        input("\n按回车键继续...")

def main():
    """主程序"""
    parser = argparse.ArgumentParser(description='Graphiti Knowledge Explorer')
    parser.add_argument('--host', default='localhost', help='Graphiti服务器地址')
    parser.add_argument('--port', type=int, default=8092, help='Graphiti服务器端口')
    parser.add_argument('--test', action='store_true', help='测试连接')
    
    args = parser.parse_args()
    
    # 创建客户端
    client = GraphitiClient(args.host, args.port)
    
    # 测试连接
    print(f"🔌 连接到 Graphiti 服务器 ({args.host}:{args.port})...")
    stats = client.get_stats()
    
    if stats['total_memories'] == 0 and stats['total_entities'] == 0:
        print("⚠️  警告: 无法获取数据，请确保服务器正在运行")
        if not args.test:
            response = input("是否继续? (y/n): ")
            if response.lower() != 'y':
                return
    else:
        print(f"✅ 连接成功! 发现 {stats['total_memories']} 条记忆, {stats['total_entities']} 个实体")
    
    if args.test:
        return
    
    # 启动浏览器
    explorer = GraphitiExplorer(client)
    try:
        explorer.run()
    except KeyboardInterrupt:
        print("\n\n👋 再见!")

if __name__ == "__main__":
    main()