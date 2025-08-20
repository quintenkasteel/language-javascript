// Real-world React component example
import React, { useState, useEffect } from 'react';
import PropTypes from 'prop-types';

const UserCard = ({ user, onEdit, className = '' }) => {
    const [isEditing, setIsEditing] = useState(false);
    const [formData, setFormData] = useState({
        name: user.name,
        email: user.email
    });

    useEffect(() => {
        setFormData({
            name: user.name,
            email: user.email
        });
    }, [user]);

    const handleSubmit = async (e) => {
        e.preventDefault();
        try {
            await onEdit(user.id, formData);
            setIsEditing(false);
        } catch (error) {
            console.error('Failed to update user:', error);
        }
    };

    const handleChange = (field) => (e) => {
        setFormData(prev => ({
            ...prev,
            [field]: e.target.value
        }));
    };

    if (isEditing) {
        return (
            <form onSubmit={handleSubmit} className={`user-card editing ${className}`}>
                <input
                    type="text"
                    value={formData.name}
                    onChange={handleChange('name')}
                    placeholder="Name"
                    required
                />
                <input
                    type="email"
                    value={formData.email}
                    onChange={handleChange('email')}
                    placeholder="Email"
                    required
                />
                <button type="submit">Save</button>
                <button type="button" onClick={() => setIsEditing(false)}>
                    Cancel
                </button>
            </form>
        );
    }

    return (
        <div className={`user-card ${className}`}>
            <h3>{user.name}</h3>
            <p>{user.email}</p>
            <button onClick={() => setIsEditing(true)}>Edit</button>
        </div>
    );
};

UserCard.propTypes = {
    user: PropTypes.shape({
        id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]).isRequired,
        name: PropTypes.string.isRequired,
        email: PropTypes.string.isRequired
    }).isRequired,
    onEdit: PropTypes.func.isRequired,
    className: PropTypes.string
};

export default UserCard;