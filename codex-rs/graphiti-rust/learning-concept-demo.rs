//! 学习检测概念演示

use std::collections::HashMap;

// 简化的学习事件类型
#[derive(Debug, Clone)]
pub enum LearningEventType {
    NewEntityType { entity_type: String, confidence: f32 },
    NewCodePattern { pattern_type: String, confidence: f32 },
    ConceptualBreakthrough { concept: String, confidence: f32 },
}

// 简化的学习事件
#[derive(Debug, Clone)]
pub struct LearningEvent {
    pub event_type: LearningEventType,
    pub summary: String,
    pub description: String,
    pub confidence: f32,
    pub impact_score: f32,
}

impl LearningEvent {
    pub fn new(event_type: LearningEventType, summary: String, description: String) -> Self {
        let confidence = match &event_type {
            LearningEventType::NewEntityType { confidence, .. } => *confidence,
            LearningEventType::NewCodePattern { confidence, .. } => *confidence,
            LearningEventType::ConceptualBreakthrough { confidence, .. } => *confidence,
        };
        
        let impact_score = confidence * 0.8;
        
        Self {
            event_type,
            summary,
            description,
            confidence,
            impact_score,
        }
    }
    
    pub fn should_notify(&self, min_confidence: f32, min_impact: f32) -> bool {
        self.confidence >= min_confidence && self.impact_score >= min_impact
    }
}

// 学习检测器
pub struct LearningDetector {
    known_entities: HashMap<String, u32>,
    known_patterns: HashMap<String, u32>,
}

impl LearningDetector {
    pub fn new() -> Self {
        Self {
            known_entities: HashMap::new(),
            known_patterns: HashMap::new(),
        }
    }
    
    pub fn detect_learning(&mut self, content: &str) -> Vec<LearningEvent> {
        let mut events = Vec::new();
        
        // 检测新实体类型
        if content.contains("struct") || content.contains("class") || content.contains("interface") {
            let entity_type = "DataStructure".to_string();
            let count = self.known_entities.entry(entity_type.clone()).or_insert(0);
            
            if *count == 0 {
                let event = LearningEvent::new(
                    LearningEventType::NewEntityType {
                        entity_type: entity_type.clone(),
                        confidence: 0.85,
                    },
                    format!("🆕 发现新实体类型: {}", entity_type),
                    format!("系统识别出新的实体类型 '{}'，这是首次遇到此类型", entity_type),
                );
                events.push(event);
            }
            *count += 1;
        }
        
        // 检测设计模式
        let content_lower = content.to_lowercase();
        if content_lower.contains("observer") && content_lower.contains("pattern") {
            let pattern = "Observer Pattern".to_string();
            let count = self.known_patterns.entry(pattern.clone()).or_insert(0);
            
            if *count == 0 {
                let event = LearningEvent::new(
                    LearningEventType::NewCodePattern {
                        pattern_type: pattern.clone(),
                        confidence: 0.9,
                    },
                    format!("🔍 识别代码模式: {}", pattern),
                    format!("在代码中发现 {} 设计模式，这是一个重要的架构概念", pattern),
                );
                events.push(event);
            }
            *count += 1;
        }
        
        if content_lower.contains("singleton") && (content_lower.contains("pattern") || content_lower.contains("instance")) {
            let pattern = "Singleton Pattern".to_string();
            let count = self.known_patterns.entry(pattern.clone()).or_insert(0);
            
            if *count == 0 {
                let event = LearningEvent::new(
                    LearningEventType::NewCodePattern {
                        pattern_type: pattern.clone(),
                        confidence: 0.85,
                    },
                    format!("🔍 识别代码模式: {}", pattern),
                    format!("检测到 {} 设计模式的使用", pattern),
                );
                events.push(event);
            }
            *count += 1;
        }
        
        // 检测概念突破
        if content_lower.contains("eureka") || 
           content_lower.contains("breakthrough") ||
           content_lower.contains("突破") ||
           content_lower.contains("明白了") ||
           content_lower.contains("finally understand") {
            let event = LearningEvent::new(
                LearningEventType::ConceptualBreakthrough {
                    concept: "Deep Understanding".to_string(),
                    confidence: 0.95,
                },
                "💡 概念突破".to_string(),
                "检测到深度理解的突破性进展，这表明学习者对某个概念有了新的认知".to_string(),
            );
            events.push(event);
        }
        
        events
    }
    
    pub fn get_stats(&self) -> (usize, usize) {
        (self.known_entities.len(), self.known_patterns.len())
    }
}

// 通知管理器
pub struct NotificationManager {
    notifications: Vec<LearningEvent>,
    total_sent: u32,
}

impl NotificationManager {
    pub fn new() -> Self {
        Self {
            notifications: Vec::new(),
            total_sent: 0,
        }
    }
    
    pub fn notify(&mut self, event: LearningEvent) {
        if event.should_notify(0.5, 0.4) {
            println!("📢 学习通知 (置信度: {:.2}, 影响: {:.2})", event.confidence, event.impact_score);
            println!("   {}", event.summary);
            println!("   📝 {}", event.description);
            println!();
            
            self.notifications.push(event);
            self.total_sent += 1;
        }
    }
    
    pub fn get_stats(&self) -> (usize, u32) {
        (self.notifications.len(), self.total_sent)
    }
}

fn main() {
    println!("🧠 Graphiti 学习检测系统概念演示");
    println!("=========================================");
    println!();
    
    let mut detector = LearningDetector::new();
    let mut notification_manager = NotificationManager::new();
    
    // 模拟学习场景
    let scenarios = vec![
        ("用户添加内存", "I just implemented a new struct for user authentication"),
        ("设计模式学习", "This code follows the observer pattern for event handling"),
        ("概念突破", "Eureka! I finally understand how Rust's ownership system works"),
        ("重复内容", "Adding another struct for data storage"),
        ("另一个模式", "Using singleton pattern for configuration management"),
        ("中文突破", "今天我突破了对异步编程的理解"),
        ("已知模式重复", "Another observer pattern implementation here"),
    ];
    
    for (i, (scenario, content)) in scenarios.iter().enumerate() {
        println!("📝 场景 {}: {}", i + 1, scenario);
        println!("   内容: {}", content);
        
        let events = detector.detect_learning(content);
        
        if events.is_empty() {
            println!("   🔍 没有检测到新的学习事件");
        } else {
            println!("   🎯 检测到 {} 个学习事件:", events.len());
            for event in events {
                notification_manager.notify(event);
            }
        }
        
        println!();
    }
    
    // 显示统计信息
    let (entity_count, pattern_count) = detector.get_stats();
    let (active_notifications, total_sent) = notification_manager.get_stats();
    
    println!("📊 学习检测统计");
    println!("===============");
    println!("🏷️  已识别实体类型: {}", entity_count);
    println!("🔍 已识别代码模式: {}", pattern_count);
    println!("📢 发送通知数量: {}", total_sent);
    println!("📋 活跃通知数量: {}", active_notifications);
    println!();
    
    println!("✅ 概念演示完成！");
    println!("💡 这展示了图谱隐式学习检测的核心能力：");
    println!("   • 🆕 检测新实体类型的出现");
    println!("   • 🔍 识别代码设计模式");
    println!("   • 💡 发现概念突破时刻");
    println!("   • 📢 智能通知过滤");
    println!("   • 📊 学习统计追踪");
}