#!/usr/bin/env python3
"""
Free Local Embedding Server for Graphiti Rust

This server provides embeddings using SentenceTransformers models,
completely free and running locally.

Usage:
    pip install sentence-transformers flask
    python embedding_server.py

The server will be available at http://localhost:8000
"""

import logging
import time
from typing import List, Dict, Any

try:
    from sentence_transformers import SentenceTransformer
    from flask import Flask, request, jsonify
except ImportError as e:
    print(f"Missing dependencies: {e}")
    print("Please install with: pip install sentence-transformers flask")
    exit(1)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

app = Flask(__name__)

# Global model cache
MODEL_CACHE: Dict[str, SentenceTransformer] = {}

# Default model configurations
DEFAULT_MODELS = {
    'sentence-transformers/all-MiniLM-L6-v2': {
        'dimension': 384,
        'description': 'Fast and efficient, good for most tasks'
    },
    'sentence-transformers/all-mpnet-base-v2': {
        'dimension': 768,
        'description': 'Higher quality, slower'
    },
    'sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2': {
        'dimension': 384,
        'description': 'Multilingual support'
    }
}

def load_model(model_name: str) -> SentenceTransformer:
    """Load and cache a SentenceTransformer model."""
    if model_name not in MODEL_CACHE:
        logger.info(f"Loading model: {model_name}")
        try:
            start_time = time.time()
            model = SentenceTransformer(model_name)
            load_time = time.time() - start_time
            MODEL_CACHE[model_name] = model
            logger.info(f"Model {model_name} loaded in {load_time:.2f} seconds")
        except Exception as e:
            logger.error(f"Failed to load model {model_name}: {e}")
            raise
    
    return MODEL_CACHE[model_name]

@app.route('/health', methods=['GET'])
def health():
    """Health check endpoint."""
    return jsonify({
        'status': 'healthy',
        'models_loaded': list(MODEL_CACHE.keys()),
        'available_models': list(DEFAULT_MODELS.keys())
    })

@app.route('/models', methods=['GET'])
def list_models():
    """List available models."""
    return jsonify({
        'available_models': DEFAULT_MODELS,
        'loaded_models': list(MODEL_CACHE.keys())
    })

@app.route('/embed', methods=['POST'])
def embed():
    """Generate embeddings for the given texts."""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'error': 'No JSON data provided'}), 400
        
        texts = data.get('texts', [])
        model_name = data.get('model', 'sentence-transformers/all-MiniLM-L6-v2')
        
        if not texts:
            return jsonify({'error': 'No texts provided'}), 400
        
        if not isinstance(texts, list):
            return jsonify({'error': 'Texts must be a list'}), 400
        
        # Validate model name
        if model_name not in DEFAULT_MODELS:
            return jsonify({
                'error': f'Unsupported model: {model_name}',
                'available_models': list(DEFAULT_MODELS.keys())
            }), 400
        
        logger.info(f"Generating embeddings for {len(texts)} texts using {model_name}")
        
        # Load model
        start_time = time.time()
        model = load_model(model_name)
        
        # Generate embeddings
        embeddings = model.encode(texts, convert_to_tensor=False, show_progress_bar=False)
        
        # Convert to list of lists
        embeddings_list = embeddings.tolist()
        
        generation_time = time.time() - start_time
        logger.info(f"Generated embeddings in {generation_time:.3f} seconds")
        
        return jsonify({
            'embeddings': embeddings_list,
            'model': model_name,
            'dimension': len(embeddings_list[0]) if embeddings_list else 0,
            'count': len(embeddings_list),
            'generation_time': generation_time
        })
        
    except Exception as e:
        logger.error(f"Error generating embeddings: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/embed/single', methods=['POST'])
def embed_single():
    """Generate embedding for a single text (convenience endpoint)."""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'error': 'No JSON data provided'}), 400
        
        text = data.get('text', '')
        model_name = data.get('model', 'sentence-transformers/all-MiniLM-L6-v2')
        
        if not text:
            return jsonify({'error': 'No text provided'}), 400
        
        # Use the batch endpoint internally
        response = embed()
        if response.status_code != 200:
            return response
            
        result = response.get_json()
        if result['embeddings']:
            return jsonify({
                'embedding': result['embeddings'][0],
                'model': result['model'],
                'dimension': result['dimension'],
                'generation_time': result['generation_time']
            })
        else:
            return jsonify({'error': 'No embedding generated'}), 500
            
    except Exception as e:
        logger.error(f"Error generating single embedding: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/similarity', methods=['POST'])
def similarity():
    """Compute similarity between texts."""
    try:
        data = request.get_json()
        
        if not data:
            return jsonify({'error': 'No JSON data provided'}), 400
        
        text1 = data.get('text1', '')
        text2 = data.get('text2', '')
        model_name = data.get('model', 'sentence-transformers/all-MiniLM-L6-v2')
        
        if not text1 or not text2:
            return jsonify({'error': 'Both text1 and text2 are required'}), 400
        
        logger.info(f"Computing similarity using {model_name}")
        
        model = load_model(model_name)
        
        # Generate embeddings
        embeddings = model.encode([text1, text2])
        
        # Compute cosine similarity
        similarity_score = float(model.similarity(embeddings[0:1], embeddings[1:2]).item())
        
        return jsonify({
            'similarity': similarity_score,
            'text1': text1,
            'text2': text2,
            'model': model_name
        })
        
    except Exception as e:
        logger.error(f"Error computing similarity: {e}")
        return jsonify({'error': str(e)}), 500

@app.errorhandler(404)
def not_found(error):
    return jsonify({'error': 'Endpoint not found'}), 404

@app.errorhandler(500)
def internal_error(error):
    return jsonify({'error': 'Internal server error'}), 500

def main():
    """Main function to run the server."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Free Local Embedding Server')
    parser.add_argument('--host', default='0.0.0.0', help='Host to bind to')
    parser.add_argument('--port', type=int, default=8000, help='Port to bind to')
    parser.add_argument('--preload', nargs='*', 
                       choices=list(DEFAULT_MODELS.keys()),
                       help='Models to preload on startup')
    parser.add_argument('--debug', action='store_true', help='Enable debug mode')
    
    args = parser.parse_args()
    
    # Preload models if requested
    if args.preload:
        for model_name in args.preload:
            logger.info(f"Preloading model: {model_name}")
            try:
                load_model(model_name)
            except Exception as e:
                logger.error(f"Failed to preload {model_name}: {e}")
    
    logger.info(f"Starting embedding server on {args.host}:{args.port}")
    logger.info(f"Available models: {list(DEFAULT_MODELS.keys())}")
    logger.info("API endpoints:")
    logger.info("  GET  /health - Health check")
    logger.info("  GET  /models - List available models")
    logger.info("  POST /embed - Generate embeddings")
    logger.info("  POST /embed/single - Generate single embedding")
    logger.info("  POST /similarity - Compute text similarity")
    
    app.run(
        host=args.host,
        port=args.port,
        debug=args.debug,
        threaded=True
    )

if __name__ == '__main__':
    main()